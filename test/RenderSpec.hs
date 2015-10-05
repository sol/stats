module RenderSpec (spec) where

import           Test.Hspec

import           Render

spec :: Spec
spec = do
  describe "renderSection" $ do
    it "renders a section" $ do
      renderSection (Section "foo" [Field "bar" "baz"]) `shouldBe` [
          "foo"
        , "  bar: baz"
        ]
  describe "renderSections" $ do
    it "interleaves sections with empty lines" $ do
      renderSections [Section "foo" [], Section "bar" []] `shouldBe` [
          "foo"
        , ""
        , "bar"
        ]
