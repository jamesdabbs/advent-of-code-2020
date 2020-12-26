[![ci](https://github.com/jamesdabbs/advent-of-code-2020/workflows/hs/badge.svg)](https://github.com/jamesdabbs/advent-of-code-2020/actions?query=workflow%3Ahs)
[![codecov](https://codecov.io/gh/jamesdabbs/advent-of-code-2020/branch/main/graph/badge.svg?token=8U4K1HOI01)](https://codecov.io/gh/jamesdabbs/advent-of-code-2020)
[![Docker Repository on Quay](https://quay.io/repository/jamesdabbs/advent-of-code-2020/status "Docker Repository on Quay")](https://quay.io/repository/jamesdabbs/advent-of-code-2020)

# Commands

```bash
$ stack exec aoc run             # run all solutions
$ stack exec aoc run $n          # run solution #n
$ stack exec aoc bench           # run all benchmarks
$ stack exec aoc bench $n        # run benchmarks for solution #n
$ stack test                     # run unit tests
$ stack test --fast --file-watch # run unit tests, watching for changes
$ env CI=true stack test         # run unit and regression tests
$ stack exec aoc list            # list auto-discovered solutions
$ stack exec aoc -- --help       # view CLI documentation
$ stack exec aoc scaffold        # scaffold out an empty solution for the next unsolved day
$ stack exec aoc scaffold $n     # scaffold out an empty solution #n
```

## Profiling

To profile an example, run

```bash
$ stack build --profile
$ stack exec --profile -- aoc run $n +RTS -p # for time profile; -h for memory
```

You may also want to maintain a separate build cache.
See https://stackoverflow.com/questions/32123475/profiling-builds-with-stack for details.

## Future Work

At this point, I've got solutions that I'm _reasonably_ happy with that each run in ~10s or less, and am planning on pausing for a bit. What follows are some `TODO`s that may or may not get done, _mostly_ here as a note for future me.

### Performance

Current benchmark times are approx

    01  1.084 ms
    02  3.691 ms
    03  27.29 ms
    04  6.519 ms
    05  1.898 ms
    06  7.178 ms
    07  16.05 ms
    08  11.98 ms
    09  1.541 ms
    10  238.1 μs
    11  8.330 s
    12  649.5 μs
    13  45.97 μs
    14  123.8 ms
    15  6.519 s
    16  9.961 ms
    17  1.011 s
    18  17.17 ms
    19  1.110 s
    20  67.67 ms
    21  4.503 ms
    22  6.239 s
    23  11.73 s
    24  2.507 s
    25  2.749 s

I'd love to get each of these in the sub-second range. Some possibilities there:

* 11: allocs ~2G over the run. We'd do better with a mutable current world state, but would need to manually hash and compare history.
* 15: may be one of the tougher ones to get < 1s, as we're already spending most of our time in readByteArray# and similar.
* 17: most of our time here is in countNeighbors, where we can do better than constructing the whole duplicate list and then counting. We also don't need history, so maintaining a mutable world state would also cut down on allocs.
* 19: (see below) I'd prefer to re-write this before optimizing
* 22: I added the subgame-result memoization as an optimization before I realized I had the prompt slightly wrong, and should measure and make sure it's actually helping. After that, should take a careful look at allocs - maybe decks should be mutable within the game and sliced to start subgames?
* 23: is going to be tough, sim 15.
* 24: sim 11, 17
* 25: there is certainly a better algorithm for mod-log

There's probably some juice to squeeze throughout with annotating for strictness, ensuring folds are appropriately directed, &c.

### Expressivity

* 17: could be an interesting one to explore type-applying the relevant dimension
* 18: would be interesting to explore prior art here
* 19: seems like there's a real clean solution out there using `Fix`
* 20: may just be the problem, but feels like there are a lot of disjointed parts here; the solution could probably be organized a little better, if nothing else

### Infrastructure

* Split out steps and phases (parsing, solving, rendering) so that we can benchmark each separately
* Rework parsing internals to use bytestring.Char8
* Auto-fetch input from web
* PR generation helpers (incl. benchmarks)
* Post benchmark times back to PR
* Improve CLI --help messages

### Common Utilities

It may be helpful (esp. for next year) to extract some common methods

* grid parsing
* run `f :: a -> a` until it stabilizes
* game of life parametrized with rules, adjacencies (11, 17, 24)
* ...
