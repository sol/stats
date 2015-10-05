{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module StatsSpec (spec) where

import           Data.List
import           Data.Maybe
import           Data.Ord
import           GHC.Stack
import           Test.Hspec
import           Test.QuickCheck

import           Stats

infix 1 `shouldBeNear`

shouldBeNear :: (?loc :: CallStack) => Double -> Double -> Expectation
actual `shouldBeNear` expected
  | lower < actual && actual < upper = return ()
  | otherwise = actual `shouldBe` expected
  where
    e = abs (0.0001 * expected)
    lower = expected - e
    upper = expected + e

type family Input a where
  Input Number = Double
  Input Text = String

data RandomStats a = RandomStats {
  randomStats :: Stats a
, randomInput :: [Maybe (Input a)]
}

deriving instance (Show (Input a), Show a) => Show (RandomStats a)

mkStats :: (Maybe (Input a) -> Stats a -> Stats a) -> Stats a -> [Maybe (Input a)] -> RandomStats a
mkStats add empty input = RandomStats (foldr add empty input) input

instance Arbitrary (RandomStats Number) where
  arbitrary = mkStats addNumberDataPoint emptyNumberStats <$> arbitrary

instance Arbitrary (RandomStats Text) where
  arbitrary = mkStats addTextDataPoint emptyTextStats <$> arbitrary

data NonEmptyInput a = NonEmptyInput (RandomStats a) [Input a]

deriving instance (Show (Input a), Show a) => Show (NonEmptyInput a)

instance Arbitrary (RandomStats a) => Arbitrary (NonEmptyInput a) where
  arbitrary = uncurry NonEmptyInput <$> statsWithValues `suchThat` (not . null . snd)
    where
      statsWithValues = do
        stats <- arbitrary
        let values = catMaybes (randomInput stats)
        return (stats, values)

spec :: Spec
spec = do
  describe "stats for all columns" $ do
    describe "statsCount" $ do
      it "returns number of data points we have seen" $ do
        property $ \ RandomStats{..} -> do
          statsCount (randomStats :: Stats Number) `shouldBe` sum [1 | Just _ <- randomInput]

    describe "statsNullCount" $ do
      it "returns number of null data points we have seen" $ do
        property $ \ RandomStats{..} -> do
          statsNullCount (randomStats :: Stats Number) `shouldBe` sum [1 | Nothing <- randomInput]

  describe "stats for number columns" $ do
    describe "statsMin" $ do
      it "returns the smallest data point we have seen" $ do
        property $ \ (NonEmptyInput RandomStats{..} values) -> do
          statsMin randomStats `shouldBe` minimum values

    describe "statsMax" $ do
      it "returns the largest data point we have seen" $ do
        property $ \ (NonEmptyInput RandomStats{..} values) -> do
          statsMax randomStats `shouldBe` maximum values

    describe "statsAverage" $ do
      it "returns the arithmetic mean" $ do
        property $ \ (NonEmptyInput RandomStats{..} values) -> do
          statsAverage randomStats `shouldBeNear` sum values / genericLength values

      context "when we haven't seen any data points" $ do
        it "returns 0" $ do
          statsAverage emptyNumberStats `shouldBe` 0

  describe "stats for text columns" $ do
    let sortByLength = sortBy (comparing (\x -> (length x, x)))
        lengthOfFirstGroup = genericLength . head . group

    describe "statsCountShortest" $ do
      it "returns the number of shortest values" $ do
        property $ \ (NonEmptyInput RandomStats{..} values) -> do
          statsCountShortest randomStats `shouldBe` (lengthOfFirstGroup . sortByLength) values

      it "breaks ties alphabetically" $ do
        let stats = foldr addTextDataPoint emptyTextStats [Just "foo", Just "bar", Just "foo"]
        statsCountShortest stats `shouldBe` 1

    describe "statsCountLongest" $ do
      it "returns the number of longest values" $ do
        property $ \ (NonEmptyInput RandomStats{..} values) -> do
          statsCountLongest randomStats `shouldBe` (lengthOfFirstGroup . reverse . sortByLength) values

    describe "statsAverageLength" $ do
      it "returns the average length of all seen data points" $ do
        property $ \ (NonEmptyInput RandomStats{..} values) -> do
          statsAverageLength randomStats `shouldBeNear` sum (map genericLength values) / genericLength values

      context "when we haven't seen any data points" $ do
        it "returns 0" $ do
          statsAverageLength emptyTextStats `shouldBe` 0
