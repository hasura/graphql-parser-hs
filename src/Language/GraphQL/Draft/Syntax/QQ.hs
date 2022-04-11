{-# LANGUAGE QuasiQuotes #-}

-- | Quasiquotation for 'Language.GraphQL.Draft.Syntax' types.
--
-- These quasiquoters can be used to construct GraphQL literal values at
-- compile-time.
module Language.GraphQL.Draft.Syntax.QQ
  ( name,
    executableDoc,
  )
where

-------------------------------------------------------------------------------

import Data.Text qualified as Text
import Language.GraphQL.Draft.Parser (parseExecutableDoc)
import Language.GraphQL.Draft.Syntax qualified as Syntax
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (lift)
import Language.Haskell.TH.Syntax.Compat (examineSplice, unTypeQQuote)
import Prelude

-------------------------------------------------------------------------------

-- | Construct 'Syntax.Name' literals at compile-time via quasiquotation.
name :: QuasiQuoter
name =
  QuasiQuoter {quoteExp, quotePat, quoteType, quoteDec}
  where
    quotePat _ = error "executableDoc does not support quoting patterns"
    quoteType _ = error "executableDoc does not support quoting types"
    quoteDec _ = error "executableDoc does not support quoting declarations"
    quoteExp = unTypeQQuote . examineSplice . Syntax.litName . Text.pack

-- | Construct @'Syntax.ExecutableDocument' 'Syntax.Name'@ literals at compile
-- time via quasiquotation.
executableDoc :: QuasiQuoter
executableDoc =
  QuasiQuoter {quoteExp, quotePat, quoteType, quoteDec}
  where
    quotePat _ = error "executableDoc does not support quoting patterns"
    quoteType _ = error "executableDoc does not support quoting types"
    quoteDec _ = error "executableDoc does not support quoting declarations"
    quoteExp s = case parseExecutableDoc (Text.pack s) of
      Left err -> fail $ show err
      Right result -> lift result
