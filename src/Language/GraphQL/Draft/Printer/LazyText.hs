module Language.GraphQL.Draft.Printer.LazyText where

import           Data.Text.Lazy                 (Text)
import           Data.Text.Lazy.Builder
import           Protolude                      hiding (Text)

import           Language.GraphQL.Draft.Printer
import           Language.GraphQL.Draft.Syntax


instance Printer Builder where
  stringP = fromString
  textP   = fromText
  charP   = singleton
  intP    = fromString . show
  doubleP = fromString . show

renderExecutableDoc :: ExecutableDocument -> Text
renderExecutableDoc = render executableDocument

render :: (a -> Builder) -> a -> Text
render f v = toLazyText $ f v
