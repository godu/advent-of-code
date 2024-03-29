# advent-of-code

[![Continuous Integration 👮‍♂️](https://github.com/godu/advent-of-code/workflows/Continuous%20Integration%20%F0%9F%91%AE%E2%80%8D%E2%99%82%EF%B8%8F/badge.svg)](https://github.com/godu/advent-of-code/actions?query=workflow%3A%22Continuous+Integration+%F0%9F%91%AE%E2%80%8D%E2%99%82%EF%B8%8F%22)

[Advent of code](https://adventofcode.com/2020)

## Install

```shell
stack install hlint ormolu apply-refact
```

## Build

```shell
stack build --test --haddock --no-haddock-hyperlink-source
```

## Test

```shell
hlint .
ormolu --color always --check-idempotence --mode check **/*.hs
stack test [--file-watch --fast] [--ta "--match \"Year20XX.DayXX\""]
```

## Run

```shell
stack run advent-of-code-20XX-exe
```
