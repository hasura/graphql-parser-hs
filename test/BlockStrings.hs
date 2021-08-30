-- | Regression tests for issue #20 https://github.com/hasura/graphql-parser-hs/issues/20

module BlockStrings where

import           Data.Attoparsec.Text
import qualified Data.Text                      as T
import qualified Data.Text.Prettyprint.Doc      as PP
import           Data.Void                      (Void)
import           Hedgehog
import           Language.GraphQL.Draft.Parser
import qualified Language.GraphQL.Draft.Printer as Printer
import           Language.GraphQL.Draft.Syntax
import qualified Prettyprinter.Render.Text      as PP

blockTest :: IO Bool
blockTest = do
  checkParallel $ Group "Test.parser.block-string.unit"
    [ ("parses the specExample", blockParsesTo "\n    Hello,\n      World!\n\n    Yours,\n      GraphQL.\n  " "Hello,\n  World!\n\nYours,\n  GraphQL.")
    , ("do not remove WS from the end of lines", blockParsesTo "\nFoo \nbar  " "Foo \nbar  ")
    , ("tabs are WS as well", blockParsesTo "\n\t\tFoo\n\t\tbar\n\t\t\tqux" "Foo\nbar\n\tqux")
    , ("tabs work with spaces", blockParsesTo "\n\t Foo\n \tbar\n\t\t qux" "Foo\nbar\n qux")
    , ("parses newline", blockParsesTo "\n" "")
    , ("parses very simples not-empty block", blockParsesTo "x" "x")
    , ("common indentation is removed", blockParsesTo "\n  a \n   b \n  c " "a \n b \nc ")
    , ("zero common indentation is possible", blockParsesTo "\na \n b \nc " "a \n b \nc ")
    , ("no whitespace is removed from the first line", blockParsesTo "  abc " "  abc ")
    , ("ignores escaping", blockParsesTo "  \\  " "  \\  ") -- this is a single \
    , ("\n in first characters is parsed", blockParsesTo "\n hey  " "hey  ")
    , ("", blockParsesTo "\nx\n" "x")
    , ("empty single line", blockParsesTo "" "")
    , ("empty two lines", blockParsesTo "\n" "")
    , ("empty three lines", blockParsesTo "\n\n" "")
    , ("empty X lines", blockParsesTo "\n\n\n\n\n\n" "")
    , ("preserves escaped newlines", blockParsesTo "\nhello\\nworld\n" "hello\\nworld")
    , ("", blockParsesTo "\n\"\n" "\"")
    , ("escaped triple-quotes are ignored as block terminator", blockParsesTo "\n   \\\"\"\"hey\n   friends\n" "\\\"\"\"hey\nfriends")
    , ("fails for normal string", blockParseFail (\t -> "\"" <> t <> "\"") "hey")
    , ("fails for block string that is not closed", blockParseFail ("\"\"\"" <>) "hey")
    , ("fails for block string that is not closed when there are escaped triple-quotes",
          blockParseFail (\t -> "\"\"\"" <> t <> "\\\"\"\"" <> t) "hey")
    , ("does not ignore escaping when its part of a escaped tripel-quotes",
          blockParseFail (\t -> "\"\"\"" <> t <> "\"\"\"") "\\") -- this is a single \, but it touches the """ at the end

    -- Printing
    , ("a line with indentation prints as blockstring", printsAsBlockString (VString "foo\n    indented!"))
    , ("when starts with indentation prints a regular string", printsAsString (VString "    indented!"))
    , ("when has escaped newlines prints as regular string", printsAsString (VString "foo\\n    indented!"))
    , ("when it has non-printable chars it prints as regular string", printsAsString (VString "\NUL\n    indented!"))
    ]

-- | We use this function to tests cases that we know should
-- fail, when we pass a function to construct wrapped the
-- body in a delimiter, where we will probably be testing
-- for errors using it.
blockParseFail :: (T.Text -> T.Text) -> T.Text -> Property
blockParseFail tripleQuoted unparsed = withTests 1 $ property $ do
  case parseOnly blockString (tripleQuoted unparsed) of
    Left _ -> success
    Right _ -> do
      footnote ("Should have failed for: " <> T.unpack (tripleQuoted unparsed))
      failure

-- | We use this to wrap the first argument with I've been
-- calling "triple-quotes", which are the delimiters for the
-- block strings.
blockParsesTo :: T.Text -> T.Text -> Property
blockParsesTo unparsed expected = withTests 1 $ property $ do
  let result =
        case parseOnly blockString (tripleQuoted unparsed) of
          Left l  -> Left ("Block parser failed: " <> T.pack l)
          Right r -> Right r
  either onError (expected ===) result
  where
    onError errorMsg = do
      footnote (T.unpack errorMsg)
      failure
    tripleQuoted t = "\"\"\"" <> t <> "\"\"\""

printsAsBlockString :: Value Void -> Property
printsAsBlockString val = withTests 1 $ property $ do
  let printed = prettyPrinter . Printer.value $ val
  footnote $ "should have been printed as a BlockString: " <> T.unpack printed
  assert ("\"\"\"" `T.isPrefixOf` printed)
  assert ("\"\"\"" `T.isSuffixOf` printed)
  where
    prettyPrinter :: PP.Doc T.Text -> T.Text
    prettyPrinter = PP.renderStrict . PP.layoutPretty PP.defaultLayoutOptions

printsAsString :: Value Void -> Property
printsAsString val = withTests 1 $ property do
  let printed = prettyPrinter . Printer.value $ val
  footnote ("should have been printed as a String: " <> T.unpack printed)
  assert (not $ "\"\"\"" `T.isPrefixOf` printed)
  where
    prettyPrinter :: PP.Doc T.Text -> T.Text
    prettyPrinter = PP.renderStrict . PP.layoutPretty PP.defaultLayoutOptions
