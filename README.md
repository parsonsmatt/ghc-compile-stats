# ghc-compile-stats

silly lil program that you pipe ur GHC/cabal output into and it tells you how long modules take t compile

the timing info will give weird results with `-j` enabled in `ghc-options` so compile in serial to get module details

but even with `-j` it's good to know how many modules you're skipping and how many you're compiling

## install

```
$ stack build
$ stack install .
```

## usage

```
$ cabal build | ghc-compile-stats
```
