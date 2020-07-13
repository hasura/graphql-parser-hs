module Language.GraphQL.Draft.Printer.Text where

import           Data.Text                      (Text)
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

  doubleP = string . show
  {-# INLINE doubleP #-}

renderExecutableDoc :: ExecutableDocument Name -> Text
renderExecutableDoc = render executableDocument

render :: (a -> Builder) -> a -> Text
render f = run . f
