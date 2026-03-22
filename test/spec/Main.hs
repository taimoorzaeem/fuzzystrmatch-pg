module Main
  ( main )
  where

import qualified Test.Hspec as HS

import qualified LevenshteinSpec

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
