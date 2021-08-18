
-- | Regression tests for issue #20 https://github.com/hasura/graphql-parser-hs/issues/20

module BlockStrings where

import           Hedgehog
import           Language.GraphQL.Draft.Parser
import qualified Data.Text as T
import           Data.Attoparsec.Text

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
    , ("single lines works like a string", blockParsesTo "  abc " "  abc ")
    , ("ignores escaping", blockParsesTo "  \\  " "  \\  ") -- this is a single \
    , ("\n in first characters is parsed", blockParsesTo "\n hey  " "hey  ")
    , ("", blockParsesTo "\nx\n" "x")
    , ("empty single line", blockParsesTo "" "")
    , ("empty two lines", blockParsesTo "\n" "")
    , ("empty three lines", blockParsesTo "\n\n" "") -- first is deleted, third is deleted
    , ("empty four lines", blockParsesTo "\n\n\n" "\n")
    , ("empty four lines", blockParsesTo "\n\n\n\n" "\n\n")
    , ("empty four lines", blockParsesTo "\n\n\n\n\n" "\n\n\n")
    , ("empty four lines", blockParsesTo "\n\n\n\n\n\n" "\n\n\n\n")
    --, ("empty three lines", blockParsesTo "\n \n \n" "")
    ]

-- note that this function is only for block strings
blockParsesTo :: T.Text -> T.Text -> Property
blockParsesTo unparsed expected =
  withTests 1 $ property $ do
    let result =
          case parseOnly blockString (tripleQuoted unparsed) of
            Left l -> Left ("Parser failed: " <> T.pack l)
            Right r -> Right r
    either onError (expected ===) result
  where
    onError errorMsg = do
      footnote (T.unpack errorMsg)
      failure

tripleQuoted :: T.Text -> T.Text
tripleQuoted t = "\"\"\"" <> t <> "\"\"\""
