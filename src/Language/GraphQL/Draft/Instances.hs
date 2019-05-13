{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.GraphQL.Draft.Instances where

import           Prelude                                 (fail)
import           Protolude

import qualified Data.Aeson                              as J
import qualified Data.Text.Lazy                          as TL

import           Language.GraphQL.Draft.Parser
import           Language.GraphQL.Draft.Printer.LazyText
import           Language.GraphQL.Draft.Syntax

instance J.FromJSON ExecutableDocument where
  parseJSON = J.withText "ExecutableDocument" $ \t ->
    case parseExecutableDoc t of
      Right a -> return a
      Left _  -> fail "parsing the graphql query failed"

instance J.ToJSON ExecutableDocument where
  toJSON = J.String . TL.toStrict . renderExecutableDoc
