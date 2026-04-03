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
levenshtein source target = levenshteinWithCosts source target 1 1 1

-- | Calculate the levenshtein distance with different costs
levenshteinWithCosts :: Text -> Text -> Int -> Int -> Int -> Int
levenshteinWithCosts source target insCost delCost subCost = runST $ do
  let sLen = T.length source
      tLen = T.length target

  prev <- MVU.new $ tLen + 1
  curr <- MVU.new $ tLen + 1

  -- TODO: If possible, refactor this iteration part, looks ugly.
  --       We are dealing with Mutable Vectors here, so maybe this is the way?

  -- Init: [0,1,2,..,tLen] for default 1 cost of insertion
  -- Init: [0,2,4,..,tLen * 2] for cost of insertion equal 2 and so on
  forLoopM_ 0 tLen (\i -> MVU.write prev i (i * insCost))

  forLoopM_ 0 (sLen - 1) $ \i -> do
    -- Init: delete cost, goes like 1,2,3... for default 1 cost of deletion
    -- Init: delete cost, goes like 2,4,6... for when cost of deletion is 2 and so on
    MVU.write curr 0 ((i + 1) * delCost)

    forLoopM_ 0 (tLen - 1) $
      calculateCurrentCostAndWrite source target prev curr i insCost delCost subCost

    -- Copy current to previous before next iteration
    MVU.copy prev curr

  -- The last one is the answer at target length position
  MVU.read prev tLen

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

-- | Return closure/function/handler to compute the values in a row
--   This reads the current cell and write the minimum cost
--   after calculating insert cost, del cost and sub cost
calculateCurrentCostAndWrite
  :: Text
  -> Text
  -> MVU.MVector s Int
  -> MVU.MVector s Int
  -> Int
  -> Int -> Int -> Int
  -> (Int -> ST s ())
calculateCurrentCostAndWrite source target prev curr i insCost delCost subCost j = do
  subCost' <- MVU.read prev j
  insCost' <- MVU.read prev (j + 1)
  delCost' <- MVU.read curr j

  -- NOTE: Although T.index is unsafe, safety can be guaranteed through
  --       outside bounds checking.
  let curCost = if T.index source i == T.index target j then 0 else subCost
      minCost  = minimum [ insCost' + insCost, delCost' + delCost, subCost' + curCost ]

  MVU.write curr (j + 1) minCost
