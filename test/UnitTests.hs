{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE QuasiQuotes #-}

module UnitTests
  ( values
  , untypedQueries
  , typedQueries
  ) where

import Protolude hiding (option)
import Test.Tasty.Hspec
import qualified Data.Attoparsec.Text          as AT
import Language.GraphQL.Draft.Syntax           as AST
import Language.GraphQL.Draft.Parser           as AST
import Test.Hspec.QuickCheck
import Test.QuickCheck (arbitrary, forAll, resize)
import qualified Data.Text as T
import qualified Data.Aeson as A
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as BL

values = do
  describe "numbers" $ do
    it "integer 1" $ parseVal "1" `shouldBe` Right (AST.VInt 1)
    prop "many integers" $ \x -> parseVal (show x) == Right (AST.VInt x)
    it "float 1.5" $ parseVal "1.5" `shouldBe` Right (AST.VFloat 1.5)
    prop "many floats" $ \x -> parseVal (show x) == Right (AST.VFloat x)
  describe "strings" $ do
    it "string \"a\"" $ parseVal "\"a\"" `shouldBe` Right (AST.VString $ AST.StringValue "a")
    prop "many strings" $ \x -> parseVal (decodeUtf8 $ BL.toStrict $ A.encode x)
      == Right (AST.VString $ AST.StringValue $ T.pack x)
  describe "lists" $ do
    it "list [1,1.4]" $ parseVal "[1,1.4]" `shouldBe` Right (list [AST.VInt 1, AST.VFloat 1.4] )
    prop "many lists of integers" $ \x -> parseVal (show x) == Right (list $ AST.VInt <$> x)
  describe "parsing objects" $
    it "works for {a:{b:2}}" $ \x -> parseVal "{a: {b: 2}}"  `shouldBe`
      Right ( obj
        [ objFld "a" $ obj
            [ objFld "b" $ AST.VInt 2 ]
        ])
  where
    obj = AST.VObject . AST.ObjectValueG
    list =  AST.VList . AST.ListValueG
    objFld n = AST.ObjectFieldG (AST.Name n)
    parseVal = AT.parseOnly value


untypedQueries = do
  this "simple query"                       "{cat}"
    $ execDoc [ operUT [selFld $ fieldN "cat"] ]

  this "two queries"                 "{cat} {dog}"
    $ execDoc
    [ operUT [selFld $ fieldN "cat"]
    , operUT [selFld $ fieldN "dog"]
    ]

  this "with integer argument"         "{ cat(age: 2) }"
    $ execDoc
    [ operUT
        [ selFld $ fieldAN "cat" [arg "age" $ AST.VInt 2] ]
    ]

  this "with object argument"  "{ cat( name: { ne : null} ) }"
    $ execDoc
    [ operUT
        [ selFld $ fieldAN "cat"
            [ arg "name" $ valObj [ objField "ne" AST.VNull ]
            ]
        ]
    ]

  forM_ [" ",","] $ \x -> this "with two arguments"
    ("{ cat(age: 2" <> x <> " city: \"bar\") }")
    $ execDoc
    [ operUT
        [ selFld $ fieldAN "cat"
            [ arg "age" $ AST.VInt 2
            , arg "city" $ vStr "bar"
            ]
        ]
    ]
  where
    parseExec = parseExecutableDoc
    selFld = AST.SelectionField
    vStr =  AST.VString . AST.StringValue
    fieldN n = AST.Field Nothing n [] [] []
    fieldAN n a = AST.Field Nothing n a [] []
    arg n = AST.Argument (AST.Name n)
    objField n =  AST.ObjectFieldG (AST.Name n)
    valObj = AST.VObject . AST.ObjectValueG
    operUT =  AST.ExecutableDefinitionOperation . AST.OperationDefinitionUnTyped
    execDoc = AST.ExecutableDocument
    this info str expected = it (info ++ " '" ++ T.unpack str ++ "' ") $
      parseExec str `shouldBe` Right expected

typedQueries = do
  describe "queries" $ do
    this "query"                       "query {cat}"
      $ execDoc [ operT query [selFldN "cat"] ]
  where
    query = AST.OperationTypeQuery
    selFldN n = AST.SelectionField $ AST.Field Nothing n [] [] []
    parseExec = parseExecutableDoc
    operT t = AST.ExecutableDefinitionOperation .
      AST.OperationDefinitionTyped . AST.TypedOperationDefinition t Nothing [] []
    execDoc = AST.ExecutableDocument
    this info str expected = it (info ++ " '" ++ T.unpack str ++ "' ") $
      parseExec str `shouldBe` Right expected
