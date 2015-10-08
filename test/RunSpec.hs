{-# LANGUAGE OverloadedStrings #-}
module RunSpec (spec) where

import           Test.Hspec
import           Data.String

import           Render
import           Stats
import           Run

spec :: Spec
spec = do
  describe "run" $ do
    it "calculates stats" $ do
      let input = (fromString . unlines) [
              "\"foo (text)\",\"bar (number)\""
            , "bar,23"
            , ",42"
            , "baz,"
            ]
      run input `shouldBe` Right [
          Section "foo (text)" [
            Field "count" "2"
          , Field "null count" "1"
          , Field "count(shortest value)" "1"
          , Field "count(longest value)" "1"
          , Field "average length" "3.0"
          , Field "distinct count (estimate)" "2"
          ]
        , Section "bar (number)" [
            Field "count" "2"
          , Field "null count" "1"
          , Field "minimum" "23.0"
          , Field "maximum" "42.0"
          , Field "average" "32.5"]
        ]

  describe "parseColumnName" $ do
    it "parses number columns" $ do
      let name = "latency (number)"
      parseColumnName name `shouldBe` Column name (NumberStats emptyNumberStats)

    it "parses text columns" $ do
      let name = "sessionId (text)"
      parseColumnName name `shouldBe` Column name (TextStats emptyTextStats)
