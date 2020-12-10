[![ci](https://github.com/jamesdabbs/advent-of-code-2020/workflows/hs/badge.svg)](https://github.com/jamesdabbs/advent-of-code-2020/actions?query=workflow%3Ahs)
[![codecov](https://codecov.io/gh/jamesdabbs/advent-of-code-2020/branch/main/graph/badge.svg?token=8U4K1HOI01)](https://codecov.io/gh/jamesdabbs/advent-of-code-2020)
[![Docker Repository on Quay](https://quay.io/repository/jamesdabbs/advent-of-code-2020/status "Docker Repository on Quay")](https://quay.io/repository/jamesdabbs/advent-of-code-2020)

# Commands

```bash
$ stack exec aoc run            # run all solutions
$ stack exec aoc run $n         # run solution #n
$ stack exec aoc bench          # run all benchmarks
$ stack exec aoc bench $n       # run benchmarks for solution #n
$ stack test                     # run unit tests
$ stack test --fast --file-watch # run unit tests, watching for changes
$ env CI=true stack test         # run unit and regression tests
$ stack exec aoc list           # list auto-discovered solutions
$ stack exec aoc -- --help      # view CLI documentation
```

## Profiling

To profile an example, run

```bash
$ stack build --profile
$ stack exec --profile -- aoc run $n +RTS -p # for time profile; -h for memory
```

You may also want to maintain a separate build cache.
See https://stackoverflow.com/questions/32123475/profiling-builds-with-stack for details.
