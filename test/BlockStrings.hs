
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
    --, ("only strip first and last empty lines", blockParsesTo "\n \n \n \n\n " " \n \n \n")
    {-
    , ("parses empty string", blockParsesTo "" "")
    , ("parses newline", blockParsesTo "\n" "")
    , ("parses newlines in empty block", blockParsesTo " \n   \n  \n            \n\n " "")
    , ("parses very simples not-empty block", blockParsesTo "x" "x")
    , ("parses block string containing a valid normal string inside", blockParsesTo "  \"i'm like a JSON string\"   " "\"i'm like a JSON string\"")
    , ("parses not-empty block with newlines", blockParsesTo " \n   \n  \n x   \n y   \n\n " "x\ny")
    , ("ignores escaping", blockParsesTo "  \\  " "\\") -- this is a single \
    , ("\n in first characters is parsed", blockParsesTo "\n hey  " "hey")
    , ("zero common indentation is possible", blockParsesTo " \naa \n  bbbb \ncccc " "aa\n  bbbb\ncccc")
    , ("common indentation is removed", blockParsesTo " \n   i have 3 \n    i have 4 \n   i also have 3 " "i have 3\n i have 4\ni also have 3")
    -}
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
