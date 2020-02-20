{-# LANGUAGE OverloadedStrings #-}

-- | Regression tests for issue #20 https://github.com/hasura/graphql-parser-hs/issues/20

module PrimitiveColumns (primitiveTests) where

import Hedgehog
import Protolude
import GHC.Base (fail)

import Language.GraphQL.Draft.Syntax
import Language.GraphQL.Draft.Parser

import qualified Language.GraphQL.Draft.Printer.Text as PP.TB
import qualified Language.GraphQL.Draft.Printer      as P

primitiveTests :: IsString s => [(s, Property)]
primitiveTests =
  [ ("property [ parse (print nameValue) == nameValue ]", propNullNameValue)
  , ("property [ parse (print nameBool) == nameBool ]",   propBoolNameValue)
  , ("property [ parse (print nameName) == nameName ]",   propNullNameName)
  ]

propNullNameValue :: Property
propNullNameValue = property $ either (fail . Protolude.show) (ast ===) astRoundTrip
  where
    astRoundTrip = (runParser value) printed
    printed      = PP.TB.render P.value ast
    ast          = VList (ListValueG { unListValue = [
                    VEnum (EnumValue{unEnumValue = Name{unName = "nullColumn"}})]})

propBoolNameValue :: Property
propBoolNameValue = property $ either (fail . Protolude.show) (ast ===) astRoundTrip
  where
    astRoundTrip = (runParser value) printed
    printed      = PP.TB.render P.value ast
    ast          = VList (ListValueG { unListValue = [
                    VEnum (EnumValue{unEnumValue = Name{unName = "trueColumn"}})]})

propNullNameName :: Property
propNullNameName = property $ either (fail . Protolude.show) (ast ===) astRoundTrip
  where
    astRoundTrip = (runParser nameParser) printed
    printed      = PP.TB.render P.nameP ast
    ast          = Name "nullColumntwo"
