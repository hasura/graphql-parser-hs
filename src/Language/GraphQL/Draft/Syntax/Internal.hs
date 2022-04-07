{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | Internal GraphQL AST functionality.
--
-- This module is primarily necessary due to an incorrect
-- @-Wredundant-constraints@ warning emitted by GHC when compiling
-- 'liftTypedHashMap'.
module Language.GraphQL.Draft.Syntax.Internal
  ( liftTypedHashMap,
  )
where

-------------------------------------------------------------------------------

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Language.Haskell.TH.Syntax (Lift, liftTyped)
-- import Language.Haskell.TH.Syntax.Compat (SpliceQ, expToSplice, liftTypedQuote, toCode, liftSplice, examineSplice)
import Language.Haskell.TH.Syntax.Compat
import Prelude

-------------------------------------------------------------------------------

-- | Lift a 'HashMap' into a Template Haskell splice via list conversion.
liftTypedHashMap ::
  ( Eq k,
    Hashable k,
    Lift k,
    Lift v
  ) =>
  HashMap k v ->
  CodeQ (HashMap k v)
liftTypedHashMap hm = liftCode $
  examineCode [||HashMap.fromList $$(HashMap.toList hm)||]
