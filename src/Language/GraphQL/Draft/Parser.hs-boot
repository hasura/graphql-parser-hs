module Language.GraphQL.Draft.Parser where

import Data.Text (Text)
import {-# SOURCE #-} Language.GraphQL.Draft.Syntax qualified as AST

parseExecutableDoc :: Text -> Either Text (AST.ExecutableDocument AST.Name)
parseSchemaDocument :: Text -> Either Text AST.SchemaDocument
