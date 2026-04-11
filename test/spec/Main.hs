module Main
  ( main )
  where

import qualified Criterion.Main as C
import qualified Test.Hspec as HS

import qualified LevenshteinSpec

import Data.FuzzyStrMatch.Levenshtein
import Test.Hspec.Runner
import Prelude

specs :: Spec
specs = do
  HS.describe "Run all tests" $ do
    LevenshteinSpec.spec

main :: IO ()
main = do
  summary <- hspecWithResult defaultConfig 
    { configColorMode = ColorAuto
    } specs
  
  putStrLn $ "Total tests: " ++ show (summaryExamples summary)
  putStrLn $ "Failures: "    ++ show (summaryFailures summary)

  -- =========
  -- Benchmark:
  -- =========
  -- TODO: Use more examples, for now this will do
  let source = "aaaaaaaaaaaaaaaaaaaaaaaaaaaa"
      target = "abababababababababababababababababababababababababababab"

  -- We evaluate to normal form, because we want full evaluation
  C.defaultMain
    [
      -- According to bench mark, a full distance calculation time = ~225 us
      -- In comparison, bounded calculation time = ~50 us

      -- Hence proving that this exits early as soon as the bound hits, I
      -- believe the difference would be even larger for bigger strings
      C.bench "levenshtein" $ C.nf (levenshtein source) target
    , C.bench "levenshteinLessEqual" $ C.nf (levenshteinLessEqual source target) 3
    ]
