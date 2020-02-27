module Language.GraphQL.Draft.Printer.Text where

import           Data.Text                     (Text)

import {-# SOURCE #-} Language.GraphQL.Draft.Syntax

renderExecutableDoc :: ExecutableDocument Name -> Text
