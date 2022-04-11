module Language.GraphQL.Draft.Syntax
  ( ExecutableDocument,
    Name,
    SchemaDocument,
  )
where

import Data.Kind (Type)

-------------------------------------------------------------------------------

type Name :: Type
data Name

type role ExecutableDocument nominal

type ExecutableDocument :: Type -> Type
data ExecutableDocument var

type SchemaDocument :: Type
data SchemaDocument
