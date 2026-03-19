{- |
Module      : Data.FuzzyStrMatch.Levenshtein
Description : Calculate Levenshtein distances between texts
Copyright   : (c) 2026 Taimoor Zaeem
License     : MIT
Maintainer  : Taimoor Zaeem <taimoorzaeem@gmail.com>
Stability   : Experimental
Portability : Portable
-}
module Data.FuzzyStrMatch.Levenshtein
  ( levenshtein
  , levenshteinWithCosts
  , levenshteinLessEqual
  , levenshteinLessEqualWithCosts
  )
where

import Data.Text (Text)
import Prelude

-- Function Signatures taken from:
--   https://www.postgresql.org/docs/current/fuzzystrmatch.html

levenshtein :: Text -> Text -> Int
levenshtein _ _ = 0

levenshteinWithCosts :: Text -> Text -> Int -> Int -> Int -> Int
levenshteinWithCosts _ _ _ _ _ = 0

levenshteinLessEqual :: Text -> Text -> Int -> Int
levenshteinLessEqual _ _ _ = 0

levenshteinLessEqualWithCosts :: Text -> Text -> Int -> Int -> Int -> Int -> Int
levenshteinLessEqualWithCosts _ _ _ _ _ _ = 0
