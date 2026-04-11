module LevenshteinSpec where

import qualified Data.Text as T

import Data.Text (Text)
import Data.FuzzyStrMatch.Levenshtein

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances () -- imports instances only
import Prelude

empty :: Text
empty = ""

kitten :: Text
kitten = "kitten"

sitting :: Text
sitting = "sitting"

-- TODO: Test very large strings
spec :: Spec
spec = do
  describe "Test Levenshtein Distance" $ do
    context "should be 0 when inputs are equal" $ do
      prop "when both are non-empty strings" $ do
        \txt -> levenshtein txt txt `shouldBe` 0

      it "when both are empty strings" $
        levenshtein empty empty `shouldBe` 0

    context "distance should be equal to expectation" $ do
      it "should be 3 between kitten and sitting" $
        levenshtein kitten sitting `shouldBe` 3

      prop "when one argument is empty, distance should be length of other arg" $ do
        \source -> levenshtein source empty == (T.length source)

    context "order of arguments should not matter" $ do
      prop "application is commutative" $
        \source target -> levenshtein source target == (levenshtein target source)

    context "levenshtein with different costs" $ do
      it "insertion cost is doubled" $
        levenshteinWithCosts empty kitten 2 1 1 `shouldBe` 12 -- 6 insertions, so we get 12

      it "deletion cost is doubled" $
        levenshteinWithCosts kitten empty 1 2 1 `shouldBe` 12 -- 6 deletions, so we get 12

      it "substitution cost is doubled" $
        levenshteinWithCosts kitten sitting 1 1 2 `shouldBe` 5 -- 2 substitutions + 1 deletion, so we get (2 * 2) + 1 = 5

      prop "if arguments order is switched, the result is same when switching insert and delete cost" $
        \source target cost -> if cost > 0 then levenshteinWithCosts source target cost 1 1 == levenshteinWithCosts target source 1 cost 1 else True


    -- ================================
    -- Tests for "levenshteinLessEqual"
    -- ================================
    context "levenshtein less equal" $ do
      prop "for all successful result, it should be less than maxD" $
        \source target maxD ->
          case levenshteinLessEqual source target maxD of
            Just d -> d <= maxD
            Nothing -> True

      prop "for all successful result, it should be equal to their levenstehin" $
        \source target maxD ->
          case levenshteinLessEqual source target maxD of
            Just d -> d == levenshtein source target
            Nothing -> True

      it "test on kitten and sitting example" $ do
        levenshteinLessEqual kitten sitting 3 `shouldBe` (Just 3)
        levenshteinLessEqual kitten sitting 2 `shouldBe` Nothing
