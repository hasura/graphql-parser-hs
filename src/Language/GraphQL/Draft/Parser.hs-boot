module Language.GraphQL.Draft.Parser
  ( parseExecutableDoc,
    parseSchemaDocument,
  )
where

-------------------------------------------------------------------------------

import Data.Text (Text)
import {-# SOURCE #-} qualified Language.GraphQL.Draft.Syntax as AST

-------------------------------------------------------------------------------

parseExecutableDoc :: Text -> Either Text (AST.ExecutableDocument AST.Name)
parseSchemaDocument :: Text -> Either Text AST.SchemaDocument
