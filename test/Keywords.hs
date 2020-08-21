{-# LANGUAGE TemplateHaskell #-}

-- | Regression tests for issue #20 https://github.com/hasura/graphql-parser-hs/issues/20

module Keywords (primitiveTests) where

import           Data.String
import           Data.Text                      (unpack)
import           Data.Void
import           Hedgehog
import           Text.Builder                   (run)

import           Language.GraphQL.Draft.Parser
import           Language.GraphQL.Draft.Syntax

import qualified Language.GraphQL.Draft.Printer as P

primitiveTests :: IsString s => [(s, Property)]
primitiveTests =
  [ ("property [ parse (print nameValue) == nameValue ]", propNullNameValue)
  , ("property [ parse (print nameBool) == nameBool ]",   propBoolNameValue)
  , ("property [ parse (print nameName) == nameName ]",   propNullNameName)
  ]


propNullNameValue :: Property
propNullNameValue = property $ either (fail . unpack) (ast ===) astRoundTrip
  where
    astRoundTrip = runParser value printed
    printed      = run $ P.value ast
    ast          = VList [VEnum $ EnumValue $$(litName "nullColumn")] :: Value Void

propBoolNameValue :: Property
propBoolNameValue = property $ either (fail . unpack) (ast ===) astRoundTrip
  where
    astRoundTrip = runParser value printed
    printed      = run $ P.value ast
    ast          = VList [VEnum $ EnumValue $$(litName "trueColumn")] :: Value Void

propNullNameName :: Property
propNullNameName = property $ either (fail . unpack) (ast ===) astRoundTrip
  where
    astRoundTrip = runParser nameParser printed
    printed      = run $ P.nameP ast
    ast          = $$(litName "nullColumntwo")
