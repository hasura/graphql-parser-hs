{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

import BlockStrings
import Control.Monad (unless)
import Data.ByteString.Builder qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding.Error qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Hedgehog
import Keywords
import Language.GraphQL.Draft.Generator
import Language.GraphQL.Draft.Parser qualified as Input
import Language.GraphQL.Draft.Printer qualified as Output
import Language.GraphQL.Draft.Syntax
import Prettyprinter qualified as PP
import Prettyprinter.Render.Text qualified as PP
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Builder qualified as TB

data TestMode = TMDev | TMQuick | TMRelease
  deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    TMQuick -> runTest 100
    TMDev -> runTest 500
    TMRelease -> runTest 1000
  where
    parseArgs = foldr parseArg TMDev
    parseArg str _ = case str of
      "quick" -> TMQuick
      "release" -> TMRelease
      _ -> TMDev

runTest :: TestLimit -> IO ()
runTest limit = do
  allGood1 <- tests limit
  allGood2 <- blockTest
  unless (allGood1 && allGood2) exitFailure

tests :: TestLimit -> IO Bool
tests nTests =
  checkParallel $
    Group "Test.printer.parser" $
      [ ("property [ parse (prettyPrint ast) == ast ]", propParserPrettyPrinter nTests),
        ("property [ parse (textBuilderPrint ast) == ast ]", propParserTextPrinter nTests),
        ("property [ parse (lazyTextBuilderPrint ast) == ast ]", propParserLazyTextPrinter nTests),
        ("property [ parse (bytestringBuilderPrint ast) == ast ]", propParserBSPrinter nTests)
      ]
        ++ Keywords.primitiveTests

propParserPrettyPrinter :: TestLimit -> Property
propParserPrettyPrinter = mkPropParserPrinter $ prettyPrinter . Output.executableDocument
  where
    prettyPrinter :: PP.Doc T.Text -> T.Text
    prettyPrinter = PP.renderStrict . PP.layoutPretty PP.defaultLayoutOptions

propParserTextPrinter :: TestLimit -> Property
propParserTextPrinter = mkPropParserPrinter $ TB.run . Output.executableDocument

propParserLazyTextPrinter :: TestLimit -> Property
propParserLazyTextPrinter = mkPropParserPrinter $ TL.toStrict . TL.toLazyText . Output.executableDocument

propParserBSPrinter :: TestLimit -> Property
propParserBSPrinter = mkPropParserPrinter $ bsToTxt . BS.toLazyByteString . Output.executableDocument

mkPropParserPrinter :: (ExecutableDocument Name -> T.Text) -> (TestLimit -> Property)
mkPropParserPrinter printer = \space ->
  withTests space $
    property $ do
      xs <- forAll genExecutableDocument
      let rendered = printer xs
      either onError (xs ===) $ Input.parseExecutableDoc rendered
  where
    onError (T.unpack -> errorMsg) = do
      footnote errorMsg
      failure

bsToTxt :: BL.ByteString -> T.Text
bsToTxt = TL.toStrict . TL.decodeUtf8With TE.lenientDecode
