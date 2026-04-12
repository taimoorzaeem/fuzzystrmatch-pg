# fuzzystrmatch-pg

[![Build](https://github.com/taimoorzaeem/fuzzystrmatch-pg/actions/workflows/build.yml/badge.svg)](https://github.com/taimoorzaeem/fuzzystrmatch-pg/actions/workflows/build.yml)

Haskell implementation of PostgreSQL extension/module [fuzzystrmatch](https://www.postgresql.org/docs/current/fuzzystrmatch.html).

## Roadmap

- [ ] Levenshtein - Implement Levenshtein distance functions

## Quick Start

```haskell
import Data.FuzzyStrMatch (levenshtein)
import Data.Text

kitten  = "kitten"  :: Text
sitting = "sitting" :: Text

ghci> levenshtein kitten sitting
3

ghci> levenshteinLessEqual kitten sitting 3
Just 3

-- Bounded version which exits early, hence much faster
ghci> levenshteinLessEqual kitten sitting 2
Nothing
```

## Benchmark

Benchmarking with `criterion` library:

```haskell
let source = "aaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    target = "abababababababababababababababababababababababababababab"
    maxD   = 3

defaultMain
  [
    bench "levenshtein" $ nf (levenshtein source) target
  , bench "levenshteinLessEqual" $ nf (levenshteinLessEqual source target) maxD
  ]
```

```
benchmarking levenshtein
time                 217.0 μs   (214.5 μs .. 219.0 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 212.9 μs   (211.4 μs .. 214.6 μs)
std dev              5.201 μs   (4.374 μs .. 6.315 μs)
variance introduced by outliers: 19% (moderately inflated)

benchmarking levenshteinLessEqual
time                 45.01 μs   (44.79 μs .. 45.29 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 45.09 μs   (44.86 μs .. 45.49 μs)
std dev              936.6 ns   (617.6 ns .. 1.526 μs)
variance introduced by outliers: 17% (moderately inflated)
```

We believe that the difference would be much more on longer strings.
