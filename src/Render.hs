module Render where

import           Data.List

type Name = String
type Value = String

data Section = Section Name [Field]
  deriving (Eq, Show)

data Field = Field Name Value
  deriving (Eq, Show)

field :: Show a => Name -> a -> Field
field name value = Field name (show value)

renderSections :: [Section] -> [String]
renderSections = intercalate [""] . map renderSection

renderSection :: Section -> [String]
renderSection (Section name fields) = name : map renderField fields

renderField :: Field -> String
renderField (Field name value) = "  " ++ name ++ ": " ++ value
