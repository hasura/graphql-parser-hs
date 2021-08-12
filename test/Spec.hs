{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}

import           Control.Monad                         (unless)
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
import qualified Language.GraphQL.Draft.Parser         as Input
import qualified Language.GraphQL.Draft.Printer        as Output
import           Language.GraphQL.Draft.Syntax

import           Keywords
import           BlockStrings

data TestMode = TMDev | TMQuick | TMRelease
  deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    TMQuick   -> runTest 100
    TMDev     -> runTest 500
    TMRelease -> runTest 1000
  where
    parseArgs = foldr parseArg TMDev
    parseArg str _ = case str of
      "quick"   -> TMQuick
      "release" -> TMRelease
      _         -> TMDev

runTest :: TestLimit -> IO ()
runTest limit = do

  allGood1 <- tests limit
  allGood2 <- blockTest
  unless (allGood1 && allGood2) exitFailure

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
  withTests space $ property $ do
    someRandomDoc <- forAll genExecutableDocument
    let rendered = printer <$> Input.parseExecutableDoc (printer someRandomDoc)
    -- the reason why we render twice is to make sure that
    -- when comparing we already "normalized" the block
    -- string, so each reparsing of the blockstring will no
    -- longer be different because the indentations are
    -- already at the minimun possible.
    let reRendered = printer <$> (Input.parseExecutableDoc =<< rendered)
    case (rendered, reRendered) of
        (Left  e, _      ) -> onError e
        (_      , Left  e) -> onError e
        (Right a, Right b) -> a === b
  where
    onError (T.unpack -> errorMsg) = footnote errorMsg >> failure

bsToTxt :: BL.ByteString -> T.Text
bsToTxt = TL.toStrict . TL.decodeUtf8With TE.lenientDecode
