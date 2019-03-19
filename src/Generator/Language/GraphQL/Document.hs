module Generator.Language.GraphQL.Document where

import           Hedgehog
import           Protolude

import qualified Hedgehog.Gen                              as Gen
import qualified Hedgehog.Range                            as Range

import           Generator.Language.GraphQL.TypeDefinition
import           Language.GraphQL.Draft.Syntax

generate :: MonadIO m => Gen a -> m a
generate = Gen.sample

genDocument :: Gen Document
genDocument =
  Document <$> Gen.list (Range.linear 1 3) genDefinition

genExecutableDocument :: Gen ExecutableDocument
genExecutableDocument =
  ExecutableDocument <$> Gen.list (Range.linear 1 3) genExecutableDefinition

genSchemaDocument :: Gen SchemaDocument
genSchemaDocument =
  SchemaDocument <$> Gen.list (Range.linear 1 5) genTypeDefinition
