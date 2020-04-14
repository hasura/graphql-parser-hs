module Language.GraphQL.Draft.Printer.ByteString where

import           Data.ByteString.Builder
import           Protolude

import qualified Data.ByteString.Lazy           as BL
import qualified Data.Text.Lazy                 as LT
import qualified Data.Text.Lazy.Encoding        as LT

import           Language.GraphQL.Draft.Printer
import           Language.GraphQL.Draft.Syntax


instance Printer Builder where
  stringP = stringUtf8
  {-# INLINE stringP #-}

  textP = fromText
  {-# INLINE textP #-}

  charP = charUtf8
  {-# INLINE charP #-}

  scientificP = stringUtf8 . show
  {-# INLINE scientificP #-}


render :: (a -> Builder) -> a -> BL.ByteString
render f = toLazyByteString . f

renderExecutableDoc :: ExecutableDocument -> BL.ByteString
renderExecutableDoc = toLazyByteString . executableDocument

renderSel :: Selection -> BL.ByteString
renderSel = toLazyByteString . selection

renderSelSet :: SelectionSet -> BL.ByteString
renderSelSet = toLazyByteString . selectionSet

fromText :: Text -> Builder
fromText = LT.encodeUtf8Builder . LT.fromStrict
