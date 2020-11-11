{-# LANGUAGE KindSignatures #-}
module Language.GraphQL.Draft.Syntax where

import Data.Kind (Type)

data Name
type role ExecutableDocument nominal
data ExecutableDocument var
type role TypedOperationDefinition representational nominal
data TypedOperationDefinition (frag :: Type -> Type) var
type role Selection representational nominal
data Selection (frag :: Type -> Type) var
type SelectionSet frag var = [Selection frag var]
data SchemaDocument
