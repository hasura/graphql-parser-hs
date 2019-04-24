module Language.GraphQL.Draft.Printer.Text where

import           Protolude
import           Text.Builder

import           Language.GraphQL.Draft.Printer
import           Language.GraphQL.Draft.Syntax


instance Printer Builder where
  stringP = string
  textP   = text
  charP   = char
  intP    = decimal
  -- TODO: fixedDouble? is there any other function that we can use?
  doubleP = fixedDouble 256

renderExecutableDoc :: ExecutableDocument -> Text
renderExecutableDoc = render executableDocument

render :: (a -> Builder) -> a -> Text
render f = run . f
