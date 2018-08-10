-- |
-- Module:      Language.GraphQL.Draft.Parser
-- Copyright:   (c) 2018 Hasura Technologies Pvt. Ltd.
-- License:     BSD3
-- Maintainer:  Vamshi Surabhi <vamshi@hasura.io>
-- Stability:   experimental
-- Portability: portable
module Language.GraphQL
  (
  -- * How to use this library
  -- $use

  -- ** Parsing GraphQL executable documents
  -- $executabledocs

  -- ** Parsing GraphQL schema
  -- $schema

  -- ** GraphQL functions
    parseExecutableDoc
  , parseSchemaDoc
  -- ** Parsers
  , executableDocument
  , schemaDocument
  , value
  ) where


import           Language.GraphQL.Draft.Parser

-- $use
-- This module exposes functions dealing with parsing GraphQL schema and executable documents.

-- $executabledocs
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Language.GraphQL (parseExecutableDoc)
-- >
-- > main = do
-- >   let ast = parseExecutableDoc "{ cat }"
-- >   either (fail . show) f ast
-- >   where
-- >     f _ = return () -- The function which uses the ast



-- $schema
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Language.GraphQL (parseSchemaDoc)
-- >
-- > main :: do
-- >   let schema = parseSchemaDoc "type cat {name: String!}"
-- >   either (fail . show) f ast
-- >   where
-- >     f _ = return () -- The function which uses the schema
