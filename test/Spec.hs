{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

import BlockStrings
import Control.Monad (unless)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import qualified Data.Text.Lazy.Encoding as TL
import Hedgehog
import Keywords
import Language.GraphQL.Draft.Generator
import qualified Language.GraphQL.Draft.Parser as Input
import qualified Language.GraphQL.Draft.Printer as Output
import Language.GraphQL.Draft.Syntax
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as PP
import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified Text.Builder as TB

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
