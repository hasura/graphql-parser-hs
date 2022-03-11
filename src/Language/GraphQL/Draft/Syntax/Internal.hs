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
import Language.Haskell.TH.Syntax (Lift, Q, TExp, liftTyped)
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
  Q (TExp (HashMap k v))
liftTypedHashMap a =
  [||HashMap.fromList $$(liftTyped $ HashMap.toList a)||]
