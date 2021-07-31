{-# LANGUAGE ViewPatterns #-}

import           Control.Monad                         (unless)
import           Control.Monad.IO.Class                (liftIO)
import           Hedgehog
import           System.Environment                    (getArgs)
import           System.Exit                           (exitFailure)

import qualified Data.ByteString.Builder               as BS
import qualified Data.ByteString.Lazy                  as BL
import qualified Data.Text                             as T
import qualified Data.Text.Encoding.Error              as TE
import qualified Data.Text.Lazy                        as TL
import qualified Data.Text.Lazy.Builder                as TL
import qualified Data.Text.Lazy.Encoding               as TL
import qualified Data.Text.Prettyprint.Doc             as PP
import qualified Data.Text.Prettyprint.Doc.Render.Text as PP
import qualified Text.Builder                          as TB

import           Language.GraphQL.Draft.Generator
import           Language.GraphQL.Draft.Parser         (parseExecutableDoc)
import           Language.GraphQL.Draft.Printer        (executableDocument)
import           Language.GraphQL.Draft.Syntax

import           Keywords


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
runTest limit = do
  allGood <- tests limit
  unless allGood exitFailure

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
propParserPrettyPrinter = mkPropParserPrinter $ prettyPrinter . executableDocument
  where
    prettyPrinter :: PP.Doc T.Text -> T.Text
    prettyPrinter = PP.renderStrict . PP.layoutPretty PP.defaultLayoutOptions

propParserTextPrinter :: TestLimit -> Property
propParserTextPrinter = mkPropParserPrinter $ TB.run . executableDocument

propParserLazyTextPrinter :: TestLimit -> Property
propParserLazyTextPrinter = mkPropParserPrinter $ TL.toStrict . TL.toLazyText . executableDocument

propParserBSPrinter :: TestLimit -> Property
propParserBSPrinter = mkPropParserPrinter $ bsToTxt . BS.toLazyByteString . executableDocument

mkPropParserPrinter :: (ExecutableDocument Name -> T.Text) -> (TestLimit -> Property)
mkPropParserPrinter printer = \space ->
  withTests space $ property $ do
    xs <- forAll genExecutableDocument
    let rendered = printer xs
    either onError (xs ===) $ parseExecutableDoc rendered
  where
    onError (T.unpack -> errorMsg) = do
      liftIO $ putStrLn errorMsg
      footnote errorMsg
      failure

bsToTxt :: BL.ByteString -> T.Text
bsToTxt = TL.toStrict . TL.decodeUtf8With TE.lenientDecode
