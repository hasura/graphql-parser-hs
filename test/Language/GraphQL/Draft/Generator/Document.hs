module Language.GraphQL.Draft.Generator.Document where

import           Hedgehog
import           Protolude

import qualified Hedgehog.Gen                                    as Gen
import qualified Hedgehog.Range                                  as Range

import           Language.GraphQL.Draft.Generator.Primitives
import           Language.GraphQL.Draft.Generator.TypeDefinition
import           Language.GraphQL.Draft.Syntax


genDocument :: Gen Document
genDocument =
  Document <$> Gen.list (Range.linear 1 3) genDefinition

genExecutableDocument :: Gen ExecutableDocument
genExecutableDocument =
  ExecutableDocument <$> Gen.list (Range.linear 1 3) genExecutableDefinition

genSchemaDocument :: Gen SchemaDocument
genSchemaDocument =
  SchemaDocument <$> Gen.list (Range.linear 1 5) genTypeDefinition
