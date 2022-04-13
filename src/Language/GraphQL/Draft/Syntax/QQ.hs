{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Quasiquotation for 'Language.GraphQL.Draft.Syntax' types.
--
-- These quasiquoters can be used to construct GraphQL literal values at
-- compile-time.
module Language.GraphQL.Draft.Syntax.QQ
  ( name,
    executableDoc,
    executableDocFmt,
  )
where

-------------------------------------------------------------------------------

import Data.Text qualified as Text
import Language.GraphQL.Draft.Parser (parseExecutableDoc)
import Language.GraphQL.Draft.Syntax qualified as Syntax
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import PyF (fmtConfig)
import PyF.Internal.QQ (toExp)
import Prelude

-------------------------------------------------------------------------------

-- | Construct 'Syntax.Name' literals at compile-time via quasiquotation.
--
-- This quasiquoter supports Python-style "f-string" interpolation.
--
-- For example, if one had some value @barLit = "bar"@ present in-scope, then
-- the following quasiquotation:
--
-- @
-- [name|foo_{barLit}|]
-- @
--
-- ...would produce a 'Syntax.Name' value with the value @foo_bar@.
name :: QuasiQuoter
name =
  QuasiQuoter {quoteExp, quotePat, quoteType, quoteDec}
  where
    quotePat _ = error "'name' does not support quoting patterns"
    quoteType _ = error "'name' does not support quoting types"
    quoteDec _ = error "'name' does not support quoting declarations"
    quoteExp str = do
      let formattedStrExpQ = toExp fmtConfig str
      [|Syntax.parseName . Text.pack $ $(formattedStrExpQ)|]

-- | Construct @'Syntax.ExecutableDocument' 'Syntax.Name'@ literals at compile
-- time via quasiquotation.
--
-- This quasiquoter does not support Python-style "f-string" interpolation,
-- unlike 'executableDocFmt', which means that GraphQL document literals can be
-- produced using their natural syntax:
--
-- @
-- [executableDoc|
-- {
--   hero {
--     name
--     age
--   }
-- }
-- |]
-- @
executableDoc :: QuasiQuoter
executableDoc =
  QuasiQuoter {quoteExp, quotePat, quoteType, quoteDec}
  where
    quotePat _ = error "'executableDoc' does not support quoting patterns"
    quoteType _ = error "'executableDoc' does not support quoting types"
    quoteDec _ = error "'executableDoc' does not support quoting declarations"
    quoteExp str = case (parseExecutableDoc . Text.pack $ str) of
      Left err -> fail . show $ err
      Right doc -> [|doc|]

-- | Construct @'Syntax.ExecutableDocument' 'Syntax.Name'@ literals at compile
-- time via quasiquotation.
--
-- Unlike 'executableDoc', this quasiquoter supports interpolation at the
-- expense of overloading the braces normally used to define GraphQL documents.
--
-- For example, if one were to write the following quasiquotation, with
-- @nameLit = "name"@ in-scope:
--
-- @
-- [executableDocFmt|
-- {{
--   hero {{
--     {nameLit}
--     age
--   }}
-- }}
-- |]
-- @
--
-- ...then the following document literal would be produced:
--
-- @
-- {
--   hero {
--     name
--     age
--   }
-- }
-- @
executableDocFmt :: QuasiQuoter
executableDocFmt =
  QuasiQuoter {quoteExp, quotePat, quoteType, quoteDec}
  where
    quotePat _ = error "'executableDocFmt' does not support quoting patterns"
    quoteType _ = error "'executableDocFmt' does not support quoting types"
    quoteDec _ = error "'executableDocFmt' does not support quoting declarations"
    quoteExp str = do
      let formattedStrExpQ = toExp fmtConfig str
          docExpQ = [|parseExecutableDoc . Text.pack $ $(formattedStrExpQ)|]
      [|either (fail . show) pure $(docExpQ)|]
