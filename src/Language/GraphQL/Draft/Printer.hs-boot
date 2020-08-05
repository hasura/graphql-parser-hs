module Language.GraphQL.Draft.Printer where

import           Data.String                   (IsString)
import qualified Text.Builder                  as Text (Builder)
import           Data.Text
import           Data.Scientific

import {-# SOURCE #-} Language.GraphQL.Draft.Syntax

class Print a
instance Print Name
class (Monoid a, IsString a) => Printer a where
  stringP  :: String -> a
  textP    :: Text -> a
  charP    :: Char -> a
  intP     :: Integer -> a
  doubleP  :: Scientific -> a

  {-# MINIMAL stringP, textP, charP, intP, doubleP #-}

  nameP    :: Name -> a
  nameP    = textP . unName

  nodeP :: (Print (frag var), Print var) => TypedOperationDefinition frag var -> a
  nodeP = node

  selectionSetP :: (Print (frag var), Print var) => SelectionSet frag var -> a
  selectionSetP = selectionSet
instance Printer Text.Builder

renderExecutableDoc :: ExecutableDocument Name -> Text
