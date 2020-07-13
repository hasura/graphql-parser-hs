-- | This module defines a printer for the @GraphQL@ language.
module Language.GraphQL.Draft.Printer.Pretty where

import           Data.Scientific
import           Data.Text
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text

import           Language.GraphQL.Draft.Printer
import           Language.GraphQL.Draft.Syntax

renderPretty :: Doc Text -> Text
renderPretty = renderStrict . layoutPretty defaultLayoutOptions

renderCompact :: Doc Text -> Text
renderCompact = renderStrict . layoutCompact

instance Pretty Name where
  pretty = pretty. unName

instance Printer (Doc Text) where
  stringP       = pretty
  {-# INLINE stringP #-}

  textP         = pretty
  {-# INLINE textP #-}

  charP         = pretty
  {-# INLINE charP #-}

  intP          = pretty
  {-# INLINE intP #-}

  doubleP sc    = pretty $ pack $ show sc
  {-# INLINE doubleP #-}

  nameP         = pretty
  {-# INLINE nameP #-}
