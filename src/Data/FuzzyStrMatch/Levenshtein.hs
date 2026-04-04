{- |
Module      : Data.FuzzyStrMatch.Levenshtein
Description : Calculate Levenshtein distances between texts
Copyright   : (c) 2026 Taimoor Zaeem
License     : MIT
Maintainer  : Taimoor Zaeem <taimoorzaeem@gmail.com>
Stability   : Experimental
Portability : Portable
-}
{-# LANGUAGE RecordWildCards #-}
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

-- | Levenshtein distance state
data LState s = LState {
    source  :: Text
  , target  :: Text
  , sLen    :: Int
  , tLen    :: Int
  , prev    :: MVU.MVector s Int
  , curr    :: MVU.MVector s Int
  , insCost :: Int
  , delCost :: Int
  , subCost :: Int
}

-- | Initialize the state
initLState :: Text -> Text -> Int -> Int -> Int -> ST s (LState s)
initLState s t ins del sub = do
  let sLen = T.length s
      tLen = T.length t
  prev <- MVU.new $ tLen + 1
  -- Init: [0,1,2,..,tLen] for default 1 cost of insertion
  -- Init: [0,2,4,..,tLen * 2] for cost of insertion equal 2 and so on
  mapFromUpto 0 tLen (\i -> MVU.write prev i (i * ins))
  curr <- MVU.new $ tLen + 1
  return $ LState s t sLen tLen prev curr ins del sub

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

-- We only have to use two vectors prev and curr for calculation:
-- In this example:
--
--          +---+---+---+---+---+---+---+
--          | S | I | T | T | I | N | G |
--      +---+---+---+---+---+---+---+---+
--      | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 |  <-- prev
--  +---|---+---+---+---+---+---+---+---+
--  | K | 1 | 1 | 2 | 3 | 4 | 5 | 6 | 7 |  <-- curr, becomes prev on next i
--  +---|---+---+---+---+---+---+---+---+
--  | I | 2 | 2 | 1 | 2 | 3 | 4 | 5 | 6 |  <-- becomes curr on next i and so on
--  .
--  .
--  .

-- | Calculate levenshtein distance between source and target string
levenshtein :: Text -> Text -> Int
levenshtein source target = levenshteinWithCosts source target 1 1 1

-- | Calculate the levenshtein distance with different costs
levenshteinWithCosts :: Text -> Text -> Int -> Int -> Int -> Int
levenshteinWithCosts s t ins del sub = runST $ do

  lState@LState{..} <- initLState s t ins del sub
  runLevenshtein lState

  MVU.read prev tLen -- The last one is the answer at target length position

-- Threshold Optimization:
--  https://en.wikipedia.org/wiki/Wagner-Fischer_algorithm#Possible_modifications

levenshteinLessEqual :: Text -> Text -> Int -> Int
levenshteinLessEqual _ _ _ = 0

levenshteinLessEqualWithCosts :: Text -> Text -> Int -> Int -> Int -> Int -> Int
levenshteinLessEqualWithCosts _ _ _ _ _ _ = 0

-- | Run Levenshtein algorithm
runLevenshtein :: LState s -> ST s ()
runLevenshtein lState@LState{..} =
  mapFromUpto 0 (sLen - 1) $ calculateCurrentCostAndWrite lState

-- | Calculate cost for all cells in the current row
calculateCurrentCostAndWrite :: LState s -> (Int -> ST s ())
calculateCurrentCostAndWrite lState@LState{..} i = do
  -- Init: delete cost, goes like 1,2,3... for default 1 cost of deletion
  -- Init: delete cost, goes like 2,4,6... for when cost of deletion is 2 and so on
  MVU.write curr 0 ((i + 1) * delCost)
  mapFromUpto 0 (tLen - 1) $ calculateCurrentCellCostAndWrite lState i
  MVU.copy prev curr -- Copy current to previous before next iteration

-- | Return closure/function/handler to compute the values in a row
--   This reads the current cell and write the minimum cost
--   after calculating insert cost, del cost and sub cost
calculateCurrentCellCostAndWrite :: LState s -> Int -> (Int -> ST s ())
calculateCurrentCellCostAndWrite LState{..} i j = do
  subCost' <- MVU.read prev j
  insCost' <- MVU.read prev (j + 1)
  delCost' <- MVU.read curr j

  -- NOTE: Although T.index is unsafe, safety can be guaranteed through
  --       outside bounds checking.

  -- If the character being compared are not equal, then it costs
  -- substitution cost.
  let curCost = if T.index source i == T.index target j then 0 else subCost
      minCost  = minimum [ insCost' + insCost, delCost' + delCost, subCost' + curCost ]

  MVU.write curr (j + 1) minCost

-- | Map over the vector from index i to j and apply f on i
mapFromUpto :: Int -> Int -> (Int -> ST s ()) -> ST s ()
-- "s" is the type signature is a "Phantom Type" which is needed by GHC
-- state monad for type checking reasons. At runtime, "s" is nothing. So,
-- dont' worry about it too much
mapFromUpto i j f
  | i > j     = return ()
  | otherwise = f i >> mapFromUpto (i+1) j f
