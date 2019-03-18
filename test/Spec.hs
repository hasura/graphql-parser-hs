{-# LANGUAGE OverloadedStrings #-}
import           Hedgehog
import           Protolude
import           System.Environment                          (getArgs)

import qualified Data.Text                                   as T
import qualified Data.Text.IO                                as T
import qualified Hedgehog.Gen                                as Gen
import qualified Hedgehog.Range                              as Range

import           Language.GraphQL.Draft.Generator.Document
import           Language.GraphQL.Draft.Generator.Primitives
import           Language.GraphQL.Draft.Parser               (parse,
                                                              parseExecutableDoc,
                                                              value)
import           Language.GraphQL.Draft.Syntax

import qualified Language.GraphQL.Draft.Printer              as PP


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runDevTest
    (x:_) -> case x of
      "prod" -> runProdTest
      _      -> runDevTest

runDevTest :: IO ()
runDevTest = void $ tests 500

runProdTest :: IO ()
runProdTest = void $ tests 1000

tests :: TestLimit -> IO Bool
tests nTests =
  checkParallel $ Group "Test.parser.printer" [
    ("prop_parser_printer", prop_parser_printer nTests)
    ]

propParserPrinter :: TestLimit -> Property
propParserPrinter space =
  withTests space $ property $ do
    xs <- forAll genExecutableDocument
    let rendered = PP.renderPretty $ PP.executableDocument xs
    --either fail (xs ===) $ parseExecutableDoc rendered
    case parseExecutableDoc rendered of
      Left e -> do
        liftIO $ print e
        fail e
      Right v -> xs === v
  where
    fail e = do
      footnote (T.unpack e)
      failure

-- prop_value :: Property
-- prop_value =
 -- withTests 1 $ property $ do
    -- xs <- forAll genValue
    -- let rendered = PP.renderPretty $ PP.value xs
    -- either fail (xs ===) $ (parse value) rendered
  -- where
    -- fail e = do
      -- footnote (T.unpack e)
      -- failure
--

-- helper functions to test in ghci
testFile :: FilePath -> IO ()
testFile fp = do
  contents <- T.readFile fp
  testP contents

testP :: Text -> IO ()
testP str = case parseExecutableDoc str of
  Left err  -> print err
  Right doc -> do
    T.putStrLn $ show doc
    T.putStrLn (T.pack "-------")
    T.putStrLn $ PP.renderPretty $ PP.executableDocument doc
    --T.putStrLn $ renderCompact $ executableDocument doc
