-- |
-- Module:      Language.GraphQL.Draft.TH
-- Copyright:   (c) 2018 Hasura Technologies Pvt. Ltd.
-- License:     BSD3
-- Maintainer:  Vamshi Surabhi <vamshi@hasura.io>
-- Stability:   experimental
-- Portability: portable
--
-- Functions to Parse Graphql schema or executable documents at compile time
--

module Language.GraphQL.Draft.TH
  ( parseSchemaDocQ
  , parseExecutableDocQ
  ) where

import           Control.Monad.Fail            (fail)
import           Protolude
import           System.FilePath               (FilePath)

import qualified Data.Text.IO                  as TI
import qualified Language.Haskell.TH.Syntax    as TH

import           Language.GraphQL.Draft.Parser

-- | Parse GraphQL schema at compile time
--
-- > {-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
-- > import  Language.GraphQL.Draft.TH     (parseSchemaDocQ)
-- > import  Language.GraphQL.Draft.Syntax (SchemaDocument)
-- >
-- > schemaFoo :: SchemaDocument
-- > schemaFoo = $(parseSchemaDocQ "type cat {name: String!}")

parseSchemaDocQ :: FilePath -> TH.Q TH.Exp
parseSchemaDocQ path = do
  TH.addDependentFile path
  content <- TH.runIO $ TI.readFile path
  case parseSchemaDoc content of
    Left err -> fail $ toS err
    Right x  -> TH.lift x

-- | Parse GraphQL executable document at compile time
--
-- > {-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
-- > import Language.GraphQL.Draft.TH      (parseExecutableDocQ)
-- > import Language.GraphQL.Draft.Syntax  (ExecutableDocument)
-- >
-- > execFoo :: ExecutableDocument
-- > execFoo = $(parseExecutableDocQ "{ cat }")


parseExecutableDocQ :: FilePath -> TH.Q TH.Exp
parseExecutableDocQ path = do
  TH.addDependentFile path
  content <- TH.runIO $ TI.readFile path
  case parseExecutableDoc content of
    Left err -> fail $ toS err
    Right x  -> TH.lift x
