{-# LANGUAGE KindSignatures #-}

module Language.GraphQL.Draft.Syntax
  ( ExecutableDocument,
    Name,
    SchemaDocument,
  )
where

-------------------------------------------------------------------------------

data Name

type role ExecutableDocument nominal

data ExecutableDocument var

data SchemaDocument
