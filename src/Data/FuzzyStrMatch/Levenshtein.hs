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

import qualified Data.Vector.Unboxed.Mutable as MVU
import qualified Data.Text as T

import Control.Monad.ST (runST, ST)
import Data.Text (Text)
import Prelude

-- Function Signatures taken from:
--   https://www.postgresql.org/docs/current/fuzzystrmatch.html

-- Algorithm:
--   https://en.wikipedia.org/wiki/Wagner–Fischer_algorithm

-- We have 3 popular vector implementation:
-- 1. Data.Vector
--      Linked-List implemenataion, allows thunks/laziness
-- 2. Data.Vector.Unboxed
--      Continuously allocated, much faster for primitive types, strict evaluation
-- 3. Data.Vector.Storable
--      C Compatible memory layout, used in FFI
--
-- Each of these also have a mutable implementation:
-- 1. Data.Vector.Mutable
-- 2. Data.Vector.Unboxed.Mutable
-- 3. Data.Vector.Storable.Mutable
--
-- Considering that we only store Int types and that we need
-- mutability + don't need laziness, the right data structure to
-- use here is "Unboxed Mutable Vector".

-- Calculation Matrix For Wagner-Fisher algorithm:
-- ==============================================
--
--  deletion cost, 1 for each by default
--        |
--        | +---+---+---+---+---+---+---+
--        v | S | I | T | T | I | N | G |
--      +---+---+---+---+---+---+---+---+
--      | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 |  <-- insertion 1 for each by default
--  +---|---+---+---+---+---+---+---+---+
--  | K | 1 | 1 | 2 | 3 | 4 | 5 | 6 | 7 |
--  +---|---+---+---+---+---+---+---+---+
--  | I | 2 | 2 | 1 | 2 | 3 | 4 | 5 | 6 |
--  +---|---+---+---+---+---+---+---+---+
--  | T | 3 | 3 | 2 | 1 | 2 | 3 | 4 | 5 |
--  +---|---+---+---+---+---+---+---+---+
--  | T | 4 | 4 | 3 | 2 | 1 | 2 | 3 | 4 |
--  +---|---+---+---+---+---+---+---+---+
--  | E | 5 | 5 | 4 | 3 | 2 | 2 | 3 | 4 |
--  +---|---+---+---+---+---+---+---+---+
--  | N | 6 | 6 | 5 | 4 | 3 | 3 | 2 | 3 |  <-- This is our answer!!
--  +---|---+---+---+---+---+---+---+---+

-- | Calculate levenshtein distance between source and target string
levenshtein :: Text -> Text -> Int
levenshtein source target = runST $ do
  let sLen = T.length source
      tLen = T.length target

  prev <- MVU.new $ tLen + 1
  curr <- MVU.new $ tLen + 1

  -- TODO: If possible, refactor this iteration part, looks ugly.
  --       We are dealing with Mutable Vectors here, so maybe this is the way?
  forLoopM_ 0 tLen (\i -> MVU.write prev i i) -- Init: [0,1,2,..,tLen]

  forLoopM_ 0 (sLen - 1) $ \i -> do
    MVU.write curr 0 (i + 1) -- init delete cost, goes like 1,2,3...

    -- TODO: Refactor and remove the inlined function
    forLoopM_ 0 (tLen - 1) $ \j -> do
      subCost <- MVU.read prev j
      insCost <- MVU.read prev (j + 1)
      delCost <- MVU.read curr j

      -- TODO: this is unsafe indexing, not cool (make it safer)
      let curCost = if T.index source i == T.index target j then 0 else 1
          minCost  = minimum [ insCost + 1, delCost + 1, subCost + curCost ]

      MVU.write curr (j + 1) minCost

    -- Copy current to previous before next iteration
    MVU.copy prev curr

  -- The last one is the answer at target length position
  MVU.read prev tLen

levenshteinWithCosts :: Text -> Text -> Int -> Int -> Int -> Int
levenshteinWithCosts _ _ _ _ _ = 0

-- Threshold Optimization:
--  https://en.wikipedia.org/wiki/Wagner-Fischer_algorithm#Possible_modifications

levenshteinLessEqual :: Text -> Text -> Int -> Int
levenshteinLessEqual _ _ _ = 0

levenshteinLessEqualWithCosts :: Text -> Text -> Int -> Int -> Int -> Int -> Int
levenshteinLessEqualWithCosts _ _ _ _ _ _ = 0

-- | Loop over the vector from index i to j and apply f on i
forLoopM_ :: Int -> Int -> (Int -> ST s ()) -> ST s ()
-- "s" is the type signature is a "Phantom Type" which is needed by GHC
-- state monad for type checking reasons. At runtime, "s" is nothing. So,
-- dont' worry about it too much
forLoopM_ i j f
  | i > j     = return ()
  | otherwise = f i >> forLoopM_ (i+1) j f
