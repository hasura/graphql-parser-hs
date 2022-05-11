{-# OPTIONS_GHC -fno-warn-deprecations #-}

{-# LANGUAGE TemplateHaskell #-}

module Language.GraphQL.Draft.ParserTest where

-------------------------------------------------------------------------------

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Hedgehog (Property, failure, footnote, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Language.GraphQL.Draft.Parser (Parser, blockString, nameParser, runParser, value)
import Language.GraphQL.Draft.Printer qualified as Printer
import Language.GraphQL.Draft.Syntax (EnumValue (..), Name, Value (..), litName)
import Prelude
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Text.Builder (Builder, run)

-------------------------------------------------------------------------------

spec_whitespace_blockstring :: Spec
spec_whitespace_blockstring = do
  it "does not remove whitespace from the ends of lines" do
    "\nFoo \nbar  " `shouldParseTo` "Foo \nbar  "

  it "treats tabs as whitespace" do
    "\n\t\tFoo\n\t\tbar\n\t\t\tqux" `shouldParseTo` "Foo\nbar\n\tqux"

  -- Tabs and spaces both count as a "piece of whitespace" at the beginning of
  -- the line. So, if one line starts with a tab and a space, and another
  -- starts with a space and a tab, then both lines start with two pieces of
  -- whitespace.
  it "treats tabs and spaces as equal whitespace" do
    "\n\t Foo\n \tbar\n\t\t qux" `shouldParseTo` "Foo\nbar\n qux"

  -- If all lines start with a common non-zero number of whitespace
  -- characters (see the comment above), the whitespace will be stripped. If,
  -- for example, two lines start with two characters, and one line starts
  -- with three, then the last line will have one whitespace character prefix
  -- after parsing.
  it "removes common whitespace" do
    "\n  a \n   b \n  c " `shouldParseTo` "a \n b \nc "

  -- If there is no common, non-zero number of whitespace characters between
  -- all lines, then no whitespace will be stripped from any.
  it "zero common indentation is possible" do
    "\na \n b \nc " `shouldParseTo` "a \n b \nc "

  it "trims newlines on either end of the string" do
    "\n\nHi\n" `shouldParseTo` "Hi"

  it "preserves whitespace prefixes on the first line" do
    "  abc " `shouldParseTo` "  abc "

  -- This test and the one above seem quite close together, so for a bit more
  -- clarity: if your first line has some whitespace followed by some
  -- non-whitespace characters, the whitespace will be preserved. If,
  -- however, your first line is blank (i.e. \n) and the /next/ line has some
  -- whitespace followed by some non-whitespace characters, /that/ whitespace
  -- will not be preserved.
  it "does not preserve subsequent whitespace if the first character is a newline" do
    "\n hey  " `shouldParseTo` "hey  "

spec_blockquotes :: Spec
spec_blockquotes = do
  it "parses the model \"Hello, World\" example" do
    let input  = "\n    Hello,\n      World!\n\n    Yours,\n      GraphQL.\n  "
        output = "Hello,\n  World!\n\nYours,\n  GraphQL."

    input `shouldParseTo` output

  it "parses non-whitespace characters" do
    "x" `shouldParseTo` "x"

  -- XXX(i-am-tom): is this... good?
  it "doesn't remove non-whitespace escaped characters" do
    "  \NULL  " `shouldParseTo` "  \NULL  "

  it "preserves escaped newlines" do
    "\nhello\\nworld\n" `shouldParseTo` "hello\\nworld"

  it "parses double quotes" do
    "\n\"\n" `shouldParseTo` "\""

  -- XXX(i-am-tom)
  --
  -- We can escape triple quotes by escaping the first quote. In other words,
  -- if we want to write """ in a block comment, we can use \""" to accomplish
  -- the same thing. The Haskell escaping here makes everything much more
  -- difficult to read.
  it "escaped triple-quotes are ignored as block terminator" do
    "\n   \\\"\"\"hey\n   friends\n" `shouldParseTo` "\"\"\"hey\nfriends"

spec_keywords :: Spec
spec_keywords = do
  it "parses names that begin with null" do
    let test :: Name
        test = $$(litName "nullColumn")

    parseAndPrint nameParser Printer.nameP test `shouldBe` Right test

  it "parses enum names that begin with null" do
    let test :: Value Void
        test = VList [VEnum (EnumValue $$(litName "nullColumn"))]

    parseAndPrint value Printer.value test `shouldBe` Right test

  it "parses enum names that begin with true" do
    let test :: Value Void
        test = VList [VEnum (EnumValue $$(litName "trueColumn"))]

    parseAndPrint value Printer.value test `shouldBe` Right test

--   it "a string containing \\NUL is handled correctly" do
--     withTests 1 propHandleNulString
--
--   it "a string containing \\n is handled correctly" do
--     withTests 1 propHandleNewlineString
--
--   it "a string containing \\x0011 is handled correctly" do
--     withTests 1 propHandleControlString
--
--   it "all unicode characters are supported" do
--     withTests 1 propHandleUnicodeCharacters
--
--   it "triple quotes is a valid string" do
--     withTests 1 propHandleTripleQuote
--
--   it "name with a suffix should be a valid name" do
--     withTests 1 propNameWithSuffix
--
-- propNullNameName :: Property
-- propNullNameName =
--   property $
--     roundtripParser nameParser Printer.nameP $$(litName "nullColumntwo")
--
-- propHandleNulString :: Property
-- propHandleNulString = property . roundtripValue $ VString "\NUL"
--
-- propHandleNewlineString :: Property
-- propHandleNewlineString = property . roundtripValue $ VString "\n"
--
-- propHandleControlString :: Property
-- propHandleControlString = property . roundtripValue $ VString "\x0011"
--
-- -- NB: 'liftTest' is explicitly used to restrict the 'for_' block to operate in
-- -- the 'Test' type (i.e. 'type Test = TestT Identity'), as opposed to 'PropertyT
-- -- IO'.  The 'Test' monad is a thinner monad stack & therefore doesn't suffer
-- -- from memory leakage caused by, among others, Hedgehog's 'TreeT', which is
-- -- used for automatic shrinking (which we don't need in this test).
-- propHandleUnicodeCharacters :: Property
-- propHandleUnicodeCharacters = property . liftTest $
--   for_ [minBound .. maxBound] $ \char ->
--     roundtripValue . VString $ singleton char
--
-- propHandleTripleQuote :: Property
-- propHandleTripleQuote = property . roundtripValue $ VString "\"\"\""
--
-- propNameWithSuffix :: Property
-- propNameWithSuffix =
--   property . roundtripValue $
--     VList [VEnum $ EnumValue (addSuffixes $$(litName "prefix") [$$(litSuffix "1suffix"), $$(litSuffix "2suffix")])]
--
-- -- | Test that a given 'Value'@ @'Void' passes round-trip tests as expected.
-- roundtripValue :: (MonadTest m) => Value Void -> m ()
-- roundtripValue = roundtripParser value Printer.value

-- | Given a parser, printer, and text builder, attempt to round-trip a piece
-- of AST. In other words, try to parse a printed value back into the original
-- value. This is used in the spec_keywords tests.
parseAndPrint :: (Eq a, Show a) => Parser a -> (a -> Builder) -> a -> Either Text a
parseAndPrint parser printer = runParser parser . run . printer

-------------------------------------------------------------------------------

-- | Check that empty blockstrings (i.e. just newlines) always parse to empty
-- strings.
hprop_empty_lines :: Property
hprop_empty_lines = property do
  n <- forAll $ Gen.int (Range.linear 0 100)
  let input = Text.replicate n "\n"

  case runParser blockString ("\"\"\"" <> input <> "\"\"\"") of
    Right r -> r === ""
    Left  l -> do
      footnote (Text.unpack l)
      failure

-------------------------------------------------------------------------------

-- | Assert that the blockstring text on the left should be parsed into the
-- text on the right.
shouldParseTo :: Text -> Text -> Expectation
shouldParseTo unparsed expected = do
  case runParser blockString ("\"\"\"" <> unparsed <> "\"\"\"") of
    Right r -> r `shouldBe` expected
    Left  l -> fail (Text.unpack l)

-- | Assert that the given blockstring text should fail to parse.
parseFailure :: Text -> Expectation
parseFailure unparsed = do
  case runParser blockString ("\"\"\"" <> unparsed <> "\"\"\"") of
    Right _ -> fail $ "Should have failed for: " <> Text.unpack ("\"\"\"" <> unparsed <> "\"\"\"")
    Left  _ -> pure ()
