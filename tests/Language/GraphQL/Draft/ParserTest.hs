{-# OPTIONS_GHC -fno-warn-deprecations #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Language.GraphQL.Draft.ParserTest where

-------------------------------------------------------------------------------

import Data.ByteString.Builder qualified as ByteString.Builder
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding.Error qualified as Text.Encoding.Error
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Builder qualified as Text.Lazy.Builder
import Data.Text.Lazy.Encoding qualified as Text.Lazy.Encoding
import Data.Void (Void)
import Hedgehog (Property, failure, footnote, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Language.GraphQL.Draft.Generator (genExecutableDocument)
import Language.GraphQL.Draft.Parser (Parser)
import Language.GraphQL.Draft.Parser qualified as Parser
import Language.GraphQL.Draft.Printer qualified as Printer
import Language.GraphQL.Draft.Syntax qualified as Syntax
import Prelude
import Prettyprinter qualified as PrettyPrinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text qualified as PrettyPrinter
import Test.Hspec (Expectation, Spec, it, shouldBe)
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
  let testValue :: Syntax.Value Void -> Expectation
      testValue x = parseAndPrint Parser.value Printer.value x `shouldBe` Right x

  it "parses names that begin with null" do
    let test :: Syntax.Name
        test = $$(Syntax.litName "nullColumn")

    parseAndPrint Parser.nameParser Printer.nameP test `shouldBe` Right test

  it "parses enum names that begin with null" do
    testValue $ Syntax.VList
      [ Syntax.VEnum (Syntax.EnumValue $$(Syntax.litName "nullColumn"))
      ]

  it "parses enum names that begin with true" do
    testValue $ Syntax.VList
      [ Syntax.VEnum (Syntax.EnumValue $$(Syntax.litName "trueColumn"))
      ]

  it "a string containing \\NUL is handled correctly" do
    testValue $ Syntax.VString "\NUL"

  it "a string containing \\n is handled correctly" do
    testValue $ Syntax.VString "\n"

  it "a string containing \\x0011 is handled correctly" do
    testValue $ Syntax.VString "\x0011"

  it "all unicode characters are supported" do
    for_ [minBound .. maxBound] \char ->
      testValue $ Syntax.VString (Text.singleton char)

  it "triple quotes is a valid string" do
    testValue $ Syntax.VString "\"\"\""

  it "name with a suffix should be a valid name" do
    testValue $ Syntax.VList
      [ Syntax.VEnum $ Syntax.EnumValue
          $ Syntax.addSuffixes $$(Syntax.litName "prefix")
              [ $$(Syntax.litSuffix "1suffix")
              , $$(Syntax.litSuffix "2suffix")
              ]
      ]


-- | Given a parser, printer, and text builder, attempt to round-trip a piece
-- of AST. In other words, try to parse a printed value back into the original
-- value. This is used in the spec_keywords tests.
parseAndPrint :: Parser a -> (a -> Builder) -> a -> Either Text a
parseAndPrint parser printer = Parser.runParser parser . run . printer

-------------------------------------------------------------------------------

-- | Check that the pretty printer outputs content that can still be parsed
-- correctly.
hprop_parser_pretty_printer :: Property
hprop_parser_pretty_printer = property do
  xs <- forAll genExecutableDocument

  let printer :: Syntax.ExecutableDocument Syntax.Name -> Text
      printer
        = PrettyPrinter.renderStrict
        . PrettyPrinter.layoutPretty @Text PrettyPrinter.defaultLayoutOptions
        . Printer.executableDocument

  Parser.parseExecutableDoc (printer xs) === Right xs

-- | Check that the text printer outputs content that can still be parsed
-- correctly.
hprop_parser_text_printer :: Property
hprop_parser_text_printer = property do
  xs <- forAll genExecutableDocument

  let printer :: Syntax.ExecutableDocument Syntax.Name -> Text
      printer = run . Printer.executableDocument

  Parser.parseExecutableDoc (printer xs) === Right xs

-- | Check that the lazy text printer outputs content that can still be parsed
-- correctly.
hprop_parser_lazy_text_printer :: Property
hprop_parser_lazy_text_printer = property do
  xs <- forAll genExecutableDocument

  let printer :: Syntax.ExecutableDocument Syntax.Name -> Text
      printer
        = Text.Lazy.toStrict
        . Text.Lazy.Builder.toLazyText
        . Printer.executableDocument

  Parser.parseExecutableDoc (printer xs) === Right xs

-- | Check that the bytestring printer outputs content that can still be parsed
-- correctly.
hprop_parser_bytestring_printer :: Property
hprop_parser_bytestring_printer = property do
  xs <- forAll genExecutableDocument

  let printer :: Syntax.ExecutableDocument Syntax.Name -> Text
      printer
        = Text.Lazy.toStrict
        . Text.Lazy.Encoding.decodeUtf8With Text.Encoding.Error.lenientDecode
        . ByteString.Builder.toLazyByteString
        . Printer.executableDocument

  Parser.parseExecutableDoc (printer xs) === Right xs

-------------------------------------------------------------------------------

-- | Check that empty blockstrings (i.e. just newlines) always parse to empty
-- strings.
hprop_empty_lines :: Property
hprop_empty_lines = property do
  n <- forAll $ Gen.int (Range.linear 0 100)
  let input = Text.replicate n "\n"

  case Parser.runParser Parser.blockString ("\"\"\"" <> input <> "\"\"\"") of
    Right r -> r === ""
    Left  l -> do
      footnote (Text.unpack l)
      failure

-------------------------------------------------------------------------------

-- | Assert that the blockstring text on the left should be parsed into the
-- text on the right.
shouldParseTo :: Text -> Text -> Expectation
shouldParseTo unparsed expected = do
  case Parser.runParser Parser.blockString ("\"\"\"" <> unparsed <> "\"\"\"") of
    Right r -> r `shouldBe` expected
    Left  l -> fail (Text.unpack l)

-- | Assert that the given blockstring text should fail to parse.
parseFailure :: Text -> Expectation
parseFailure unparsed = do
  case Parser.runParser Parser.blockString ("\"\"\"" <> unparsed <> "\"\"\"") of
    Right _ -> fail $ "Should have failed for: " <> Text.unpack ("\"\"\"" <> unparsed <> "\"\"\"")
    Left  _ -> pure ()
