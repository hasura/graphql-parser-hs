module Language.GraphQL.Draft.Printer where

import Data.Scientific
import Data.String (IsString)
import Data.Text
import {-# SOURCE #-} Language.GraphQL.Draft.Syntax
import qualified Text.Builder as Text (Builder)

class Print a

instance Print Name

class (Monoid a, IsString a) => Printer a where
  textP :: Text -> a
  charP :: Char -> a
  intP :: Integer -> a
  doubleP :: Scientific -> a

  {-# MINIMAL textP, charP, intP, doubleP #-}

  nameP :: Name -> a
  nameP = textP . unName

  nodeP :: (Print (frag var), Print var) => TypedOperationDefinition frag var -> a
  nodeP = node

  selectionSetP :: (Print (frag var), Print var) => SelectionSet frag var -> a
  selectionSetP = selectionSet

instance Printer Text.Builder

renderExecutableDoc :: ExecutableDocument Name -> Text
