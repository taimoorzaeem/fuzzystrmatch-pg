# fuzzystrmatch-pg

[![Build](https://github.com/taimoorzaeem/fuzzystrmatch-pg/actions/workflows/build.yml/badge.svg)](https://github.com/taimoorzaeem/fuzzystrmatch-pg/actions/workflows/build.yml)

Haskell implementation of PostgreSQL extension/module [fuzzystrmatch](https://www.postgresql.org/docs/current/fuzzystrmatch.html).

## Roadmap

- [ ] Levenshtein - Implement Levenshtein distance functions

## Quick Start

```haskell
import Data.FuzzyStrMatch (levenshtein)
import Data.Text

kitten = "kitten" :: Text

sitting = "sitting" :: Text

ghci> levenshtein kitten sitting
3
```
