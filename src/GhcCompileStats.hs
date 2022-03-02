{-# language StrictData, RecordWildCards #-}
{-# language GeneralizedNewtypeDeriving, DerivingStrategies #-}

module GhcCompileStats where

import Prelude

import Chronos hiding (getTime, now)
import Data.Foldable
import System.Clock
import qualified Data.List as List
import Data.Semigroup
import qualified Data.Text as Text
import Control.Monad.Reader
import Data.Set (Set)
import qualified Data.Set as Set
import Conduit
import Data.IORef
import Data.Char

data GhcCompileStats = GhcCompileStats
    { gcsFirstModule :: String
    , gcsModulesCompiled :: Int
    , gcsModulesSkipped :: Int
    , gcsTotalModules :: Int
    , gcsTemplateHaskellModules :: Int
    , gcsModuleTimeSpecs :: Set ModuleTimeSpec
    }

data ModuleTimeSpec = ModuleTimeSpec
    { moduleTimeSpec :: TimeSpec
    , moduleNumber :: Int
    , moduleName :: String
    }
    deriving (Eq, Ord, Show)

data GhcStatsEnv = GhcStatsEnv
    { gseModules :: Set ModuleTimeSpec
    , gseMinimumModule :: Min Int
    , gseMaximumModule :: Max Int
    , gseTotalModules :: Max Int
    , gseCompiledModuleCount :: Sum Int
    , gseLastModuleTimestamp :: Max TimeSpec
    , gseLastModuleName :: Last String
    }
    deriving Show

instance Semigroup GhcStatsEnv where
    g0 <> g1 =
        GhcStatsEnv
            { gseModules =
                gseModules g0 <> gseModules g1
            , gseMinimumModule =
                gseMinimumModule g0 <> gseMinimumModule g1
            , gseMaximumModule =
                gseMaximumModule g0 <> gseMaximumModule g1
            , gseCompiledModuleCount =
                gseCompiledModuleCount g0 <> gseCompiledModuleCount g1
            , gseLastModuleTimestamp =
                gseLastModuleTimestamp g0 <> gseLastModuleTimestamp g1
            , gseLastModuleName =
                gseLastModuleName g0 <> gseLastModuleName g1
            , gseTotalModules =
                gseTotalModules g0 <> gseTotalModules g1
            }

instance Monoid GhcStatsEnv where
    mempty =
        GhcStatsEnv
            { gseModules =
                mempty
            , gseMinimumModule =
                mempty
            , gseMaximumModule =
                mempty
            , gseCompiledModuleCount =
                mempty
            , gseLastModuleTimestamp =
                mempty
            , gseLastModuleName =
                Last "<no prior module>"
            , gseTotalModules =
                mempty
            }

newGhcStatsEnv :: IO (IORef GhcStatsEnv)
newGhcStatsEnv = do
    nsecs <- getTime Monotonic
    newIORef mempty { gseLastModuleTimestamp = Max nsecs }

runModuleStatsM :: ModuleStatsM a -> IO a
runModuleStatsM action = do
    ghcStatsEnv <- newGhcStatsEnv
    runReaderT (unModuleStatsM action) ghcStatsEnv

evalModuleStatsM :: ModuleStatsM a -> IO GhcStatsEnv
evalModuleStatsM action = do
    ghcStatsEnv <- newGhcStatsEnv
    runReaderT (unModuleStatsM action) ghcStatsEnv
    readIORef ghcStatsEnv

newtype ModuleStatsM a = ModuleStatsM
    { unModuleStatsM :: ReaderT (IORef GhcStatsEnv) IO a
    }
    deriving newtype
        (Functor, Applicative, Monad, MonadIO, MonadReader (IORef GhcStatsEnv), MonadUnliftIO, MonadThrow)

tell :: GhcStatsEnv -> ModuleStatsM ()
tell gse = do
    env <- ask
    liftIO $ atomicModifyIORef' env (\a -> (a <> gse, ()))

getLastTimestamp :: ModuleStatsM TimeSpec
getLastTimestamp = do
    gseRef <- ask
    gse <- liftIO $ readIORef gseRef
    pure $ getMax $ gseLastModuleTimestamp gse

type ModuleStream i o r = ConduitT i o ModuleStatsM r

-- | An extremely dumb program. Pipe the output of a @cabal build@ to this
-- function, and it'll record the time of each line, record how many
-- modules were skipped, etc.
--
-- If you care about how long individual modules take to compile, disable
-- parallelism. Otherwise modules will appear on the list as they are
-- started, but the next module will not necessarily appear when the prior
-- one has completed.
ghcCompileStatsMain :: IO ()
ghcCompileStatsMain = do
    ghcStatsEnv <-
        newGhcStatsEnv
    runConduit $ transPipe (\a -> runReaderT (unModuleStatsM a) ghcStatsEnv)
        $ stdinC
        .| linesUnboundedAsciiC
        .| decodeUtf8C
        .| mapC Text.unpack
        .| mapM_C parseLine
    ghcStats <- readIORef ghcStatsEnv
    putStrLn " * * * Module Statistics * * * "
    putStrLn ""
    for_ (gseModules ghcStats) $ \ModuleTimeSpec {..} -> do
        let
            readableTime =
                Text.unpack
                    $ encodeTimespan SubsecondPrecisionAuto
                    $ Timespan
                    $ fromInteger
                    $ toNanoSecs moduleTimeSpec
        logs
            [ "Module: ["
            , moduleName
            , "] compiled in "
            , readableTime
            ]
    let compiledModuleCount =
            getSum $ gseCompiledModuleCount ghcStats
    logs
        [ "Modules Compiled: "
        , show compiledModuleCount
        ]
    let biggestModuleNumber =
            getMax $ gseMaximumModule ghcStats
        smallestModuleNumber =
            getMin $ gseMinimumModule ghcStats
        totalModules =
            getMax $ gseTotalModules ghcStats
        skippedAfter =
            totalModules - biggestModuleNumber
        rangeForCompilation =
            biggestModuleNumber - smallestModuleNumber + 1
        skippedModules =
            skippedAfter + rangeForCompilation - compiledModuleCount

    logs
        [ "Modules skipped: "
        , show skippedModules
        ]

logs :: MonadIO m => [String] -> m ()
logs = liftIO . putStrLn . mconcat

now :: ModuleStatsM TimeSpec
now = liftIO $ getTime Monotonic

parseLine :: String -> ModuleStatsM ()
parseLine initialLine = do
    currentTime <- now
    logs [initialLine]
    case initialLine of
        '[' : rest -> do
            case reads rest of
                (currentModuleNumber, (' ' : 'o' : 'f' : ' ' : rest')) : _ -> do
                    tell mempty
                        { gseMinimumModule =
                            Min currentModuleNumber
                        , gseMaximumModule =
                            Max currentModuleNumber
                        , gseCompiledModuleCount =
                            Sum 1
                        }

                    case reads rest' of
                        (totalModules, (']' : ' ' : rest'')) : _ -> do
                            tell mempty
                                { gseTotalModules = Max totalModules
                                }

                            case List.stripPrefix "Compiling " rest'' of
                                Just rest''' -> do
                                    let
                                        (moduleName, rest'''') =
                                            break isSpace rest'''
                                    previousTimestamp <- getLastTimestamp
                                    let
                                        moduleTimeSpec =
                                            ModuleTimeSpec
                                                { moduleTimeSpec =
                                                    diffTimeSpec currentTime previousTimestamp
                                                , moduleNumber =
                                                    currentModuleNumber
                                                , moduleName =
                                                    moduleName
                                                }
                                    tell mempty
                                        { gseModules =
                                            Set.singleton moduleTimeSpec
                                        , gseLastModuleTimestamp =
                                            Max currentTime
                                        , gseLastModuleName =
                                            Last moduleName
                                        }


                                Nothing ->
                                    logs
                                        [ "Failed to get module name from: "
                                        , rest''
                                        ]

                _ ->
                    logs
                        [ "Failed to parse line: "
                        , initialLine
                        ]

        _ ->
            pure ()
