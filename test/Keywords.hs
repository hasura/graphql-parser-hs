{-# LANGUAGE TemplateHaskell #-}

-- | Regression tests for issue #20 https://github.com/hasura/graphql-parser-hs/issues/20

module Keywords (primitiveTests) where

import           Data.String
import           Data.Text                      (unpack)
import           Data.Void
import           Hedgehog
import           Text.Builder                   (Builder, run)

import           Language.GraphQL.Draft.Parser
import           Language.GraphQL.Draft.Syntax

import qualified Language.GraphQL.Draft.Printer as P


primitiveTests :: IsString s => [(s, Property)]
primitiveTests =
  [ ("a \"null\" prefix doesn't prevent parsing a name", propNullNameName)
  , ("a \"null\" prefix doesn't prevent parsing an enum name", propNullNameValue)
  , ("a \"true\" prefix doesn't prevent parsing an enum name", propBoolNameValue)
  , ("a string containing \\NUL is handled correctly", propHandleNulString)
  , ("a string containing \\n is handled correctly", propHandleNewlineString)
  , ("a string containing \\x0011 is handled correctly", propHandleControlString)
  ]


propNullNameValue :: Property
propNullNameValue = testRoundTripValue $ VList [VEnum $ EnumValue $$(litName "nullColumn")]

propBoolNameValue :: Property
propBoolNameValue = testRoundTripValue $ VList [VEnum $ EnumValue $$(litName "trueColumn")]

propNullNameName :: Property
propNullNameName = testRoundTrip nameParser P.nameP $$(litName "nullColumntwo")

propHandleNulString :: Property
propHandleNulString = testRoundTripValue $ VString "\NUL"

propHandleNewlineString :: Property
propHandleNewlineString = testRoundTripValue $ VString "\n"

propHandleControlString :: Property
propHandleControlString = testRoundTripValue $ VString "\x0011"


testRoundTripValue :: Value Void -> Property
testRoundTripValue = testRoundTrip value P.value

testRoundTrip
  :: (Show a, Eq a)
  => Parser a
  -> (a -> Builder)
  -> a
  -> Property
testRoundTrip parser printer ast =
  property $ either onError (ast ===) astRoundTrip
  where
    astRoundTrip = runParser parser printed
    printed      = run $ printer ast
    onError e = do
      footnote $ show printed
      fail $ unpack e
