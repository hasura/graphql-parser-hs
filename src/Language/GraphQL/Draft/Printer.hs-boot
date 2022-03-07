module Language.GraphQL.Draft.Printer
  ( renderExecutableDoc,
  )
where

-------------------------------------------------------------------------------

import Data.Text (Text)
import {-# SOURCE #-} Language.GraphQL.Draft.Syntax
  ( ExecutableDocument,
    Name,
  )

-------------------------------------------------------------------------------

renderExecutableDoc :: ExecutableDocument Name -> Text
