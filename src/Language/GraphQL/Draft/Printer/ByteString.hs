module Language.GraphQL.Draft.Printer.ByteString where

import qualified Data.ByteString.Lazy           as BL
import qualified Data.Text.Lazy                 as LT
import qualified Data.Text.Lazy.Encoding        as LT

import           Data.ByteString.Builder
import           Data.Text                      (Text)

import           Language.GraphQL.Draft.Printer

instance Printer Builder where
  stringP = stringUtf8
  {-# INLINE stringP #-}

  textP = fromText
  {-# INLINE textP #-}

  charP = charUtf8
  {-# INLINE charP #-}

  intP = int32Dec
  {-# INLINE intP #-}

  doubleP = doubleDec
  {-# INLINE doubleP #-}


render :: (a -> Builder) -> a -> BL.ByteString
render f = toLazyByteString . f

fromText :: Text -> Builder
fromText = LT.encodeUtf8Builder . LT.fromStrict
