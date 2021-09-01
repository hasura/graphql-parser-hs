{-# LANGUAGE TemplateHaskell #-}

-- | Regression tests for issue #20 https://github.com/hasura/graphql-parser-hs/issues/20

module Keywords (primitiveTests) where

import           Data.Foldable                  (for_)
import           Data.String
import           Data.Text                      (singleton, unpack)
import           Data.Void
import           Hedgehog
import           Text.Builder                   (Builder, run)

import           Language.GraphQL.Draft.Parser
import           Language.GraphQL.Draft.Syntax

import qualified Language.GraphQL.Draft.Printer as P


primitiveTests :: IsString s => [(s, Property)]
primitiveTests =
  [ ("a \"null\" prefix doesn't prevent parsing a name", withTests 1 propNullNameName)
  , ("a \"null\" prefix doesn't prevent parsing an enum name", withTests 1 propNullNameValue)
  , ("a \"true\" prefix doesn't prevent parsing an enum name", withTests 1 propBoolNameValue)
  , ("a string containing \\NUL is handled correctly", withTests 1 propHandleNulString)
  , ("a string containing \\n is handled correctly", withTests 1 propHandleNewlineString)
  , ("a string containing \\x0011 is handled correctly", withTests 1 propHandleControlString)
  , ("all unicode characters are supported", withTests 1 propHandleUnicodeCharacters)
  , ("triple quotes is a valid string", withTests 1 propHandleTripleQuote)
  ]


propNullNameValue :: Property
propNullNameValue = property $ testRoundTripValue $ VList [VEnum $ EnumValue $$(litName "nullColumn")]

propBoolNameValue :: Property
propBoolNameValue = property $ testRoundTripValue $ VList [VEnum $ EnumValue $$(litName "trueColumn")]

propNullNameName :: Property
propNullNameName = property $ testRoundTrip nameParser P.nameP $$(litName "nullColumntwo")

propHandleNulString :: Property
propHandleNulString = property $ testRoundTripValue $ VString "\NUL"

propHandleNewlineString :: Property
propHandleNewlineString = property $ testRoundTripValue $ VString "\n"

propHandleControlString :: Property
propHandleControlString = property $ testRoundTripValue $ VString "\x0011"

propHandleUnicodeCharacters :: Property
propHandleUnicodeCharacters = property $ for_ [minBound..maxBound] \c ->
  testRoundTripValue $ VString $ singleton c

propHandleTripleQuote :: Property
propHandleTripleQuote = property $ testRoundTripValue $ VString "\"\"\""

testRoundTripValue :: Value Void -> PropertyT IO ()
testRoundTripValue = testRoundTrip value P.value

testRoundTrip
  :: (Show a, Eq a)
  => Parser a
  -> (a -> Builder)
  -> a
  -> PropertyT IO ()
testRoundTrip parser printer ast = either onError (ast ===) astRoundTrip
  where
    astRoundTrip = runParser parser printed
    printed      = run $ printer ast
    onError e = do
      footnote $ show printed
      fail $ unpack e
