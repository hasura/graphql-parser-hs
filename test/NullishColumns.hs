{-# LANGUAGE OverloadedStrings #-}

module NullishColumns where

import           Hedgehog
import           Protolude
import           System.Environment                        (getArgs)

import qualified Data.ByteString.Lazy                      as BL
import qualified Data.Text                                 as T
import qualified Data.Text.Encoding.Error                  as TE
import qualified Data.Text.Lazy                            as TL
import qualified Data.Text.Lazy.Encoding                   as TL

import           Language.GraphQL.Draft.Generator.Document
import           Language.GraphQL.Draft.Parser             (parseExecutableDoc)
import           Language.GraphQL.Draft.Syntax

import qualified Language.GraphQL.Draft.Printer.ByteString as PP.BB
import qualified Language.GraphQL.Draft.Printer.LazyText   as PP.TLB
import qualified Language.GraphQL.Draft.Printer.Pretty     as PP
import qualified Language.GraphQL.Draft.Printer.Text       as PP.TB

-- import Text.Groom

import Prelude hiding (print)


propNullWorks :: Property
propNullWorks = property $ either (fail . Protolude.show) (ast ===) astRoundTrip
  where
    -- Protolude.putStrLn (groom parsed) >> print text >> either (print) (print . PP.TB.renderExecutableDoc) parsed
    -- "mutation  { insert_testTable(on_conflict: {update_columns:[nullColumntwo]}) { affected_rows  } }"
    astRoundTrip = parseExecutableDoc printed
    printed      = PP.TB.renderExecutableDoc ast
    ast          = (ExecutableDocument{getExecutableDefinitions =
                        [ExecutableDefinitionOperation
                           (OperationDefinitionTyped
                              (TypedOperationDefinition{_todType = OperationTypeMutation,
                                                        _todName = Nothing,
                                                        _todVariableDefinitions = [],
                                                        _todDirectives = [],
                                                        _todSelectionSet =
                                                          [SelectionField
                                                             (Field{_fAlias = Nothing,
                                                                    _fName = Name{unName = "insert_testTable"},
                                                                    _fArguments =
                                                                      [Argument{_aName = Name{unName = "on_conflict"},
                                                                                _aValue =
                                                                                  VObject
                                                                                    (ObjectValueG{unObjectValue
                                                                                                    =
                                                                                                    [ObjectFieldG{_ofName = Name{unName = "update_columns"},
                                                                                                                  _ofValue
                                                                                                                    =
                                                                                                                    VList
                                                                                                                      (ListValueG{unListValue
                                                                                                                                    =
                                                                                                                                    [ VEnum
                                                                                                                                       (EnumValue{unEnumValue
                                                                                                                                                    =
                                                                                                                                                    Name{unName = "nullColumntwo"}})]})}]})}],
                                                                    _fDirectives = [],
                                                                    _fSelectionSet =
                                                                      [SelectionField
                                                                         (Field{_fAlias = Nothing,
                                                                                _fName =
                                                                                  Name{unName =
                                                                                         "affected_rows"},
                                                                                _fArguments = [],
                                                                                _fDirectives = [],
                                                                                _fSelectionSet =
                                                                                  []})]})]}))]})

