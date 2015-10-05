{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Run (
  run

-- exported for testing
, Column(..)
, ColumnStats(..)
, parseColumnName
) where

import           Control.Monad
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as B
import           Data.Csv hiding (decode, decodeByName, header, record, Field)
import qualified Data.Csv as Csv
import           Data.Csv.Streaming
import           Data.Foldable

import           Render hiding (Name)
import           Stats

run :: LB.ByteString -> Either String [Section]
run input = map renderColumnStats <$> processInput input

data Column = Column {
  columnName :: Name
, columnStats :: ColumnStats
} deriving (Eq, Show)

data ColumnStats = NumberStats !(Stats Number) | TextStats !(Stats Text)
  deriving (Eq, Show)

processInput :: LB.ByteString -> Either String [Column]
processInput input = do
  header <- decodeHeader
  eachRecord (decode HasHeader input) header processRow
  where
    decodeHeader :: Either String [Column]
    decodeHeader = parseHeader . fst <$>
      (decodeByName input :: Either String (Header, Records NamedRecord))

processRow :: Record -> [Column] -> Either String [Column]
processRow record = zipWithM processCell (toList record)

processCell :: Csv.Field -> Column -> Either String Column
processCell v Column{..} = Column columnName <$> do
  case columnStats of
    NumberStats stats -> NumberStats . (`addNumberDataPoint` stats) <$> value
    TextStats stats -> TextStats . (`addTextDataPoint` stats) <$> value
  where
    value :: FromField a => Either String a
    value = runParser (parseField v)

eachRecord :: Records a -> b -> (a -> b -> Either String b) -> Either String b
eachRecord records acc0 action = go records acc0
  where
    go xs acc = case xs of
      Cons (Left err) _ -> Left err
      Cons (Right entry) ys -> action entry acc >>= go ys
      Nil err _ -> mapM_ Left err >> return acc

parseHeader :: Header -> [Column]
parseHeader = map parseColumnName . toList

parseColumnName :: Name -> Column
parseColumnName name
  | "(number)" `B.isSuffixOf` name = Column name (NumberStats emptyNumberStats)
  | otherwise = Column name (TextStats emptyTextStats)

renderColumnStats :: Column -> Section
renderColumnStats Column{..} = Section (B.unpack columnName) $ case columnStats of
  NumberStats stats -> renderNumberStats stats
  TextStats stats -> renderTextStats stats

renderNumberStats :: Stats Number -> [Field]
renderNumberStats stats = renderCommonStats stats ++ [
    field "minimum" (statsMin stats)
  , field "maximum" (statsMax stats)
  , field "average" (statsAverage stats)
  ]

renderTextStats :: Stats Text -> [Field]
renderTextStats stats = renderCommonStats stats ++ [
    field "count(shortest value)" (statsCountShortest stats)
  , field "count(longest value)" (statsCountLongest stats)
  , field "average length" (statsAverageLength stats)
  ]

renderCommonStats :: Stats a -> [Field]
renderCommonStats stats = [
    field "count" (statsCount stats)
  , field "null count" (statsNullCount stats)
  ]
