{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Stats (
  Stats
, statsCount
, statsNullCount

, Number
, emptyNumberStats
, addNumberDataPoint

, statsMin
, statsMax
, statsAverage

, Text
, emptyTextStats
, addTextDataPoint

, statsCountShortest
, statsCountLongest
, statsAverageLength
, statsDistinctCount
) where

import           Data.HyperLogLog
import           Data.Approximate.Type
import           Data.List

data Stats a = Stats {
  statsCount :: !Integer
, statsNullCount :: !Integer
, statsAdditionalData :: !a
} deriving (Eq, Show)

emptyStats :: a -> Stats a
emptyStats = Stats 0 0

addDataPoint :: (a -> b -> b) -> Maybe a -> Stats b -> Stats b
addDataPoint f mValue stats@Stats{..} = maybe skip add mValue
  where
    skip = stats {statsNullCount = succ statsNullCount}

    add value = stats {
      statsCount = succ statsCount
    , statsAdditionalData = f value statsAdditionalData
    }

data Number = Number {
  numberSum :: !Double
, numberMin :: !Double
, numberMax :: !Double
} deriving (Eq, Show)

emptyNumberStats :: Stats Number
emptyNumberStats = emptyStats $ Number 0 infinity (negate infinity)
  where
    infinity :: Double
    infinity = 1 / 0

addNumberDataPoint :: Maybe Double -> Stats Number -> Stats Number
addNumberDataPoint = addDataPoint $ \ value Number{..} -> Number {
  numberSum = numberSum + value
, numberMin = min numberMin value
, numberMax = max numberMax value
}

statsMin :: Stats Number -> Double
statsMin = numberMin . statsAdditionalData

statsMax :: Stats Number -> Double
statsMax = numberMax . statsAdditionalData

statsAverage :: Stats Number -> Double
statsAverage Stats{..}
 | statsCount == 0 = 0
 | otherwise = numberSum statsAdditionalData / fromInteger statsCount

data CountOccurrence = None | Seen !String !Integer
  deriving (Eq, Show)

data Text = Text {
  textSumLength :: !Integer
, textCountShortest :: !CountOccurrence
, textCountLongest :: !CountOccurrence
, textCountDistinct :: !(HyperLogLog $(10))
} deriving (Eq, Show)

emptyTextStats :: Stats Text
emptyTextStats = emptyStats $ Text 0 None None mempty

addTextDataPoint :: Maybe String -> Stats Text -> Stats Text
addTextDataPoint = addDataPoint $ \ value Text{..} -> Text {
    textSumLength = textSumLength + genericLength value
  , textCountShortest = count (<) value textCountShortest
  , textCountLongest = count (>) value textCountLongest
  , textCountDistinct = Data.HyperLogLog.insert value textCountDistinct
  }
  where
    count op value old = case old of
      None -> new
      Seen current n
        | value == current -> Seen current (succ n)
        | (length value, value) `op` (length current, current) -> new
        | otherwise -> old
      where
        new = Seen value 1

occurrence :: CountOccurrence -> Integer
occurrence count = case count of
  None -> 0
  Seen _ n -> n

statsCountShortest :: Stats Text -> Integer
statsCountShortest = occurrence . textCountShortest . statsAdditionalData

statsCountLongest :: Stats Text -> Integer
statsCountLongest = occurrence . textCountLongest . statsAdditionalData

statsAverageLength :: Stats Text -> Double
statsAverageLength Stats{..}
  | statsCount == 0 = 0
  | otherwise = fromInteger (textSumLength statsAdditionalData) / fromInteger statsCount

statsDistinctCount :: Stats Text -> Integer
statsDistinctCount stats = case size . textCountDistinct . statsAdditionalData $ stats of
  Approximate _ _ x _ -> fromIntegral x
