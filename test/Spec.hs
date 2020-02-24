{-# LANGUAGE OverloadedStrings #-}

import           Hedgehog
import           Protolude
import           System.Environment                        (getArgs)

import qualified Data.ByteString.Lazy                      as BL
import qualified Data.Text                                 as T
import qualified Data.Text.Encoding.Error                  as TE
import qualified Data.Text.Lazy                            as TL
import qualified Data.Text.Lazy.Encoding                   as TL

import           Language.GraphQL.Draft.Generator.Document
import           Language.GraphQL.Draft.Parser             (parseExecutableDoc)
import           Language.GraphQL.Draft.Syntax

import Keywords

import qualified Language.GraphQL.Draft.Printer.ByteString as PP.BB
import qualified Language.GraphQL.Draft.Printer.LazyText   as PP.TLB
import qualified Language.GraphQL.Draft.Printer.Pretty     as PP
import qualified Language.GraphQL.Draft.Printer.Text       as PP.TB

data TestMode = TMDev | TMQuick | TMRelease
  deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    TMQuick   -> runTest 20
    TMDev     -> runTest 100
    TMRelease -> runTest 1000
  where
    parseArgs = foldr parseArg TMDev
    parseArg str _ = case str of
      "quick"   -> TMQuick
      "release" -> TMRelease
      _         -> TMDev

runTest :: TestLimit -> IO ()
runTest = void . tests

tests :: TestLimit -> IO Bool
tests nTests =
  checkParallel $ Group "Test.printer.parser" $
    [ ("property [ parse (prettyPrint ast) == ast ]", propParserPrettyPrinter nTests)
    , ("property [ parse (textBuilderPrint ast) == ast ]", propParserTextPrinter nTests)
    , ("property [ parse (lazyTextBuilderPrint ast) == ast ]", propParserLazyTextPrinter nTests)
    , ("property [ parse (bytestringBuilderPrint ast) == ast ]", propParserBSPrinter nTests)
    ]
    ++ Keywords.primitiveTests

propParserPrettyPrinter :: TestLimit -> Property
propParserPrettyPrinter = mkPropParserPrinter PP.renderExecutableDoc

propParserTextPrinter :: TestLimit -> Property
propParserTextPrinter = mkPropParserPrinter PP.TB.renderExecutableDoc

propParserLazyTextPrinter :: TestLimit -> Property
propParserLazyTextPrinter =
  mkPropParserPrinter (TL.toStrict . PP.TLB.renderExecutableDoc)

propParserBSPrinter :: TestLimit -> Property
propParserBSPrinter =
  mkPropParserPrinter (bsToTxt . PP.BB.renderExecutableDoc)

mkPropParserPrinter :: (ExecutableDocument -> Text) -> (TestLimit -> Property)
mkPropParserPrinter printer = \space ->
  withTests space $ property $ do
    xs <- forAll genExecutableDocument
    let rendered = printer xs
    either (\e -> print e >> fail e) (xs ===) $ parseExecutableDoc rendered
  where
    fail e = footnote (T.unpack e) >> failure

bsToTxt :: BL.ByteString -> T.Text
bsToTxt = TL.toStrict . TL.decodeUtf8With TE.lenientDecode
