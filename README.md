![Continuous Integration 👮‍♂️](https://github.com/godu/advent-of-code-2020/workflows/Continuous%20Integration%20%F0%9F%91%AE%E2%80%8D%E2%99%82%EF%B8%8F/badge.svg)

# advent-of-code-2020

[Advent of code](https://adventofcode.com/2020)



## Install

```shell
$ stack install hlint ormolu
```

## Test

```shell
$ hlint .
$ ormolu --color always --check-idempotence --mode check **/*.hs
$ stack test [--file-watch --fast]
```

## Run

```shell
$ stack run
```
