module Language.GraphQL.Draft.Printer.Text where

import           Protolude
import           Text.Builder

import           Language.GraphQL.Draft.Printer
import           Language.GraphQL.Draft.Syntax


instance Printer Builder where
  stringP = string
  {-# INLINE stringP #-}

  textP   = text
  {-# INLINE textP #-}

  charP   = char
  {-# INLINE charP #-}

  intP    = decimal
  {-# INLINE intP #-}

  floatP    = text . show
  {-# INLINE floatP #-}

renderExecutableDoc :: ExecutableDocument -> Text
renderExecutableDoc = render executableDocument

render :: (a -> Builder) -> a -> Text
render f = run . f
