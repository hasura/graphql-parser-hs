-- | Regression tests for issue #20 https://github.com/hasura/graphql-parser-hs/issues/20

module BlockStrings where

import qualified Data.Text                     as T
import           Hedgehog
import           Language.GraphQL.Draft.Parser

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
    , ("escaped triple-quotes are ignored as block terminator", blockParsesTo "\n   \\\"\"\"hey\n   friends\n" "\"\"\"hey\nfriends")
    , ("fails for normal string", blockParseFail (\t -> "\"" <> t <> "\"") "hey")
    , ("fails for block string that is not closed", blockParseFail ("\"\"\"" <>) "hey")
    , ("fails for block string that is not closed when there are escaped triple-quotes",
          blockParseFail (\t -> "\"\"\"" <> t <> "\\\"\"\"" <> t) "hey")
    , ("does not ignore escaping when it's part of an escaped triple-quotes",
          blockParseFail (\t -> "\"\"\"" <> t <> "\"\"\"") "\\") -- this is a single \, but it touches the """ at the end
    ]

-- | We use this function to tests cases that we know should
-- fail, when we pass a function to construct wrapped the
-- body in a delimiter, where we will probably be testing
-- for errors using it.
blockParseFail :: (T.Text -> T.Text) -> T.Text -> Property
blockParseFail tripleQuoted unparsed = withTests 1 $ property $ do
  case runParser blockString (tripleQuoted unparsed) of
    Left _ -> success
    Right _ -> do
      footnote ("Should have failed for: " <> T.unpack (tripleQuoted unparsed))
      failure

-- | Test whether certain block string content parses to the expected value.
blockParsesTo :: T.Text -> T.Text -> Property
blockParsesTo unparsed expected = withTests 1 $ property $ do
  case runParser blockString ("\"\"\"" <> unparsed <> "\"\"\"") of
    Right r -> expected === r
    Left l  -> do
      footnote $ T.unpack $ "Block parser failed: " <> l
      failure
