{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Language.GraphQL.Draft.ParserTest where

-------------------------------------------------------------------------------

import Data.Text (Text)
import Data.Text qualified as Text
import Hedgehog (Property, failure, footnote, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Prelude
-- import Text.RawString.QQ (r)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Language.GraphQL.Draft.Parser (blockString, runParser)

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

spec_unit_tests :: Spec
spec_unit_tests = do
  it "parses the model \"Hello, World\" example" do
    let input  = "\n    Hello,\n      World!\n\n    Yours,\n      GraphQL.\n  "
        output = "Hello,\n  World!\n\nYours,\n  GraphQL."

    input `shouldParseTo` output

  it "parses non-whitespace characters" do
    "x" `shouldParseTo` "x"

  -- XXX(i-am-tom)
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

-------------------------------------------------------------------------------

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

shouldParseTo :: Text -> Text -> Expectation
shouldParseTo unparsed expected = do
  case runParser blockString ("\"\"\"" <> unparsed <> "\"\"\"") of
    Right r -> r `shouldBe` expected
    Left  l -> fail (Text.unpack l)

parseFailure :: Text -> Expectation
parseFailure unparsed = do
  case runParser blockString ("\"\"\"" <> unparsed <> "\"\"\"") of
    Right _ -> fail $ "Should have failed for: " <> Text.unpack ("\"\"\"" <> unparsed <> "\"\"\"")
    Left  _ -> pure ()
