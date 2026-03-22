module LevenshteinSpec where

import qualified Data.Text as T

import Data.Text (Text)
import Data.FuzzyStrMatch.Levenshtein (levenshtein)

import Test.Hspec
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
      it "when both are non-empty strings" $ do
        levenshtein kitten kitten `shouldBe` 0
        levenshtein sitting sitting `shouldBe` 0

      it "when both are empty strings" $
        levenshtein empty empty `shouldBe` 0

    context "distance should be equal to expectation" $ do
      it "should be 3 between kitten and sitting" $
        levenshtein kitten sitting `shouldBe` 3

      it "when one argument is empty, distance should be length of other arg" $ do
        levenshtein kitten empty `shouldBe` (T.length kitten)
        levenshtein sitting empty `shouldBe` (T.length sitting)
        levenshtein empty kitten `shouldBe` (T.length kitten)
        levenshtein empty sitting `shouldBe` (T.length sitting)

    context "order of arguments should not matter" $ do
      -- Hmm, this looks like a property test. Maybe use QuickCheck?
      -- Should be smth like: forall a b. levenshtein a b == levenshtein b a
      it "application is communtative" $
        levenshtein kitten sitting `shouldBe` (levenshtein sitting kitten)
