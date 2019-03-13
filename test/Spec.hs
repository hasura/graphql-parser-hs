import           Protolude

import           Language.GraphQL.Draft.Parser (parseExecutableDoc)
import           Language.GraphQL.Draft.Print

import qualified Data.Text.IO                  as T


main :: IO ()
main = putStrLn "Test suite not yet implemented"

testFile :: FilePath -> IO ()
testFile fp = do
  contents <- T.readFile fp
  test contents

test :: Text -> IO ()
test str = case parseExecutableDoc str of
  Left err  -> print err
  Right doc -> do
    T.putStrLn $ renderPretty $ executableDocument doc
    T.putStrLn $ renderCompact $ executableDocument doc
