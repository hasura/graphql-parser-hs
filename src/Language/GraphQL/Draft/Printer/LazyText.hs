module Language.GraphQL.Draft.Printer.LazyText where

import           Data.Text.Lazy                 (Text)
import           Data.Text.Lazy.Builder
import           Protolude                      hiding (Text)

import           Language.GraphQL.Draft.Printer
import           Language.GraphQL.Draft.Syntax


instance Printer Builder where
  stringP = fromString
  {-# INLINE stringP #-}

  textP   = fromText
  {-# INLINE textP #-}

  charP   = singleton
  {-# INLINE charP #-}

  intP    = fromString . show
  {-# INLINE intP #-}

  floatP  = fromString . show
  {-# INLINE floatP #-}

renderExecutableDoc :: ExecutableDocument -> Text
renderExecutableDoc = render executableDocument

render :: (a -> Builder) -> a -> Text
render f = toLazyText . f
