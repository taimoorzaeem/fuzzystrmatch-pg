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
  , maxD    :: Maybe Int
}

-- | Initialize the state
initLState :: Text -> Text -> Int -> Int -> Int -> Maybe Int -> ST s (LState s)
initLState s t ins del sub maxD = do
  let sLen = T.length s
      tLen = T.length t
      -- We always keep the larger string on the x-axis, helpes implementation
      -- For costs, we have to switch ins and del cost when source >= target
      sourceLenGreater = sLen >= tLen
      (s',t',sLen',tLen',ins',del') =
        if sourceLenGreater
          then (t,s,tLen,sLen,del,ins)
          else (s,t,sLen,tLen,ins,del)

  prev <- MVU.new $ tLen' + 1
  -- Init: [0,1,2,..,tLen] for default 1 cost of insertion
  -- Init: [0,2,4,..,tLen * 2] for cost of insertion equal 2 and so on
  _ <- mapFromUpto 0 tLen' (\i -> do
    MVU.write prev i (i * ins')
    return False
    )

  curr <- MVU.new $ tLen' + 1
  return $ LState s' t' sLen' tLen' prev curr ins' del' sub maxD

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

  lState@LState{..} <- initLState s t ins del sub Nothing
  _ <- runLevenshtein lState

  MVU.read prev tLen -- The last one is the answer at target length position

-- | Run Levenshtein algorithm
runLevenshtein :: LState s -> ST s Bool
runLevenshtein lState@LState{..} = do
  exit <- mapFromUpto 0 (sLen - 1) $ calculateCurrentCostAndWrite lState
  if exit then return True else return False

-- | Calculate cost for all cells in the current row
calculateCurrentCostAndWrite :: LState s -> (Int -> ST s Bool)
calculateCurrentCostAndWrite lState@LState{..} i = do
  -- Init: delete cost, goes like 1,2,3... for default 1 cost of deletion
  -- Init: delete cost, goes like 2,4,6... for when cost of deletion is 2 and so on
  MVU.write curr 0 ((i + 1) * delCost)
  exit <- mapFromUpto 0 (tLen - 1) $ calculateCurrentCellCostAndWrite lState i
  MVU.copy prev curr -- Copy current to previous before next iteration
  if exit then return True else return False

-- | Return closure/function/handler to compute the values in a row
--   This reads the current cell and write the minimum cost
--   after calculating insert cost, del cost and sub cost
calculateCurrentCellCostAndWrite :: LState s -> Int -> (Int -> ST s Bool)
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

  return $ case maxD of
    Nothing -> False
    Just n  -> minCost > n && i == (j+1) -- if we are on the diagonal and the minCost > maxD, we break

-- | Map over the vector from index i to j and apply f on i
mapFromUpto :: Int -> Int -> (Int -> ST s Bool) -> ST s Bool
-- "s" is the type signature is a "Phantom Type" which is needed by GHC
-- state monad for type checking reasons. At runtime, "s" is nothing. So,
-- dont' worry about it too much
mapFromUpto i j f
  | i > j     = return False
  | otherwise = do
    exit <- f i
    if exit then return True else mapFromUpto (i+1) j f


-- ========================
-- Threshold Optimization:
--  https://en.wikipedia.org/wiki/Wagner-Fischer_algorithm#Possible_modifications
-- =========================
levenshteinLessEqual :: Text -> Text -> Int -> Maybe Int
levenshteinLessEqual source target = levenshteinLessEqualWithCosts source target 1 1 1

levenshteinLessEqualWithCosts :: Text -> Text -> Int -> Int -> Int -> Int -> Maybe Int
levenshteinLessEqualWithCosts s t ins del sub maxD' = runST $ do

  lState@LState{..} <- initLState s t ins del sub (Just maxD')
  exit <- runLevenshtein lState

  if exit -- If exit if True, that means we have broken early, return Nothing
    then return Nothing
    else do
      -- An edge case we need to check, if the iterations are complete,
      -- we calculate the cost all the way for last row and then check
      -- if this is greater than maxD
      --        | +---+---+---+---+---+---+---+
      --        v | S | I | T | T | I | N | G |
      --      +---+---+---+---+---+---+---+---+
      --      | 0 |   |   |   |   |   |   |   |
      --  +---|---+---+---+---+---+---+---+---+
      --  | K | 1 | 1 |   |   |   |   |   |   |
      --  +---|---+---+---+---+---+---+---+---+
      --  | I | 2 | 2 | 1 | 2 | 3 | 4 | 5 | 6 | <- the diagnoal ends with cost 1
      --  +---|---+---+---+---+---+---+---+---+    but inserstion costs are added
      --
      -- Transpose case for this:
      --        | +---+---+---+
      --        v | S | I | T | -- We DON'T need to implement this case,
      --      +---+---+---+---+ -- because we always choose the longer string
      --      | 0 |   |   |   | -- to be target INTERNALLY.
      --  +---|---+---+---+---+
      --  | K | 1 | 1 |   |   |
      --  +---|---+---+---+---+
      --  | I | 2 | 2 | 1 | 2 |
      --  +---|---+---+---+---+
      --  | T | 3 | 3 | 2 | 1 | <- exit here, no need to calculate further
      --  +---|---+---+---+---+
      --  | T | 4 | 4 | 3 | 2 |
      --  +---|---+---+---+---+
      --  | E | 5 | 5 | 4 | 3 |
      --  +---|---+---+---+---+
      --  | N | 6 | 6 | 5 | 4 |  <-- This is our answer, but we need to exit early
      --  +---|---+---+---+---+
      d <- MVU.read prev tLen
      return $ if d > maxD' then Nothing else Just d
