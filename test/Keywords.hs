{-# LANGUAGE TemplateHaskell #-}

-- | Regression tests for issue #20 https://github.com/hasura/graphql-parser-hs/issues/20
module Keywords (primitiveTests) where

import Data.Foldable (for_)
import Data.String
import Data.Text (singleton, unpack)
import Data.Void
import Hedgehog
import Language.GraphQL.Draft.Parser
import qualified Language.GraphQL.Draft.Printer as P
import Language.GraphQL.Draft.Syntax
import Text.Builder (Builder, run)

primitiveTests :: IsString s => [(s, Property)]
primitiveTests =
  [ ("a \"null\" prefix doesn't prevent parsing a name", withTests 1 propNullNameName),
    ("a \"null\" prefix doesn't prevent parsing an enum name", withTests 1 propNullNameValue),
    ("a \"true\" prefix doesn't prevent parsing an enum name", withTests 1 propBoolNameValue),
    ("a string containing \\NUL is handled correctly", withTests 1 propHandleNulString),
    ("a string containing \\n is handled correctly", withTests 1 propHandleNewlineString),
    ("a string containing \\x0011 is handled correctly", withTests 1 propHandleControlString),
    ("all unicode characters are supported", withTests 1 propHandleUnicodeCharacters),
    ("triple quotes is a valid string", withTests 1 propHandleTripleQuote)
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

-- NB: 'liftTest' is explicitly used to restrict the 'for_' block to operate in
-- the 'Test' type (i.e. 'type Test = TestT Identity'), as opposed to 'PropertyT
-- IO'.  The 'Test' monad is a thinner monad stack & therefore doesn't suffer
-- from memory leakage caused by, among others, Hedgehog's 'TreeT', which is
-- used for automatic shrinking (which we don't need in this test).
propHandleUnicodeCharacters :: Property
propHandleUnicodeCharacters = property $
  liftTest $ for_ [minBound .. maxBound] \c ->
    testRoundTripValue $ VString $ singleton c

propHandleTripleQuote :: Property
propHandleTripleQuote = property $ testRoundTripValue $ VString "\"\"\""

testRoundTripValue :: MonadTest m => Value Void -> m ()
testRoundTripValue = testRoundTrip value P.value

testRoundTrip ::
  (Show a, Eq a, MonadTest m) =>
  Parser a ->
  (a -> Builder) ->
  a ->
  m ()
testRoundTrip parser printer ast = either onError (ast ===) astRoundTrip
  where
    astRoundTrip = runParser parser printed
    printed = run $ printer ast
    onError e = do
      footnote $ show printed
      footnote $ unpack e
      failure
