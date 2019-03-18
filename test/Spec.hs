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
import           Language.GraphQL.Draft.Parser               (parseExecutableDoc)
import           Language.GraphQL.Draft.Syntax

import qualified Language.GraphQL.Draft.Printer              as PP


data TestMode = TMDev | TMQuick | TMProd
  deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    TMQuick -> runTest 20
    TMDev   -> runTest 100
    TMProd  -> runTest 1000
  where
    parseArgs = foldr parseArg TMDev
    parseArg str _ = case str of
      "quick" -> TMQuick
      "prod"  -> TMProd
      _       -> TMDev

runTest :: TestLimit -> IO ()
runTest = void . tests

tests :: TestLimit -> IO Bool
tests nTests =
  checkParallel $ Group "Test.printer.parser"
    [ ("property [ parse (print ast) == ast ]", propParserPrinter nTests)
    ]

propParserPrinter :: TestLimit -> Property
propParserPrinter space =
  withTests space $ property $ do
    xs <- forAll genExecutableDocument
    let rendered = PP.renderPretty $ PP.executableDocument xs
    either (\e -> print e >> fail e) (xs ===) $ parseExecutableDoc rendered
  where
    fail e = footnote (T.unpack e) >> failure
