{-# LANGUAGE OverloadedStrings #-}

-- | Regression tests for issue #20 https://github.com/hasura/graphql-parser-hs/issues/20

module Keywords (primitiveTests) where

import           GHC.Base                            (fail)
import           Hedgehog
import           Protolude

import           Language.GraphQL.Draft.Parser
import           Language.GraphQL.Draft.Syntax

import qualified Language.GraphQL.Draft.Printer      as P
import qualified Language.GraphQL.Draft.Printer.Text as PP.TB

primitiveTests :: IsString s => [(s, Property)]
primitiveTests =
  [ ("property [ parse (print nameValue) == nameValue ]", propNullNameValue)
  , ("property [ parse (print nameBool) == nameBool ]",   propBoolNameValue)
  , ("property [ parse (print nameName) == nameName ]",   propNullNameName)
  ]

propNullNameValue :: Property
propNullNameValue = property $ either (fail . Protolude.show) (ast ===) astRoundTrip
  where
    astRoundTrip = runParser value printed
    printed      = PP.TB.render P.value ast
    ast          = VList (ListValueG { unListValue = [
                    VEnum (EnumValue{unEnumValue = Name{unName = "nullColumn"}})]})

propBoolNameValue :: Property
propBoolNameValue = property $ either (fail . Protolude.show) (ast ===) astRoundTrip
  where
    astRoundTrip = runParser value printed
    printed      = PP.TB.render P.value ast
    ast          = VList (ListValueG { unListValue = [
                    VEnum (EnumValue{unEnumValue = Name{unName = "trueColumn"}})]})

propNullNameName :: Property
propNullNameName = property $ either (fail . Protolude.show) (ast ===) astRoundTrip
  where
    astRoundTrip = runParser name printed
    printed      = PP.TB.render P.nameP ast
    ast          = Name "nullColumntwo"
