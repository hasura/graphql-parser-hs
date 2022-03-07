import Control.Monad
import Data.ByteString.Builder qualified as BS
import Data.Functor ((<&>))
import Data.Maybe (catMaybes)
import Data.String (fromString)
import Data.Text qualified as T
import Data.Text.Lazy.Builder qualified as TL
import Data.Text.Prettyprint.Doc qualified as PP
import Data.Text.Prettyprint.Doc.Render.Text qualified as PP
import Language.GraphQL.Draft.Generator
import Language.GraphQL.Draft.Parser (parseExecutableDoc)
import Language.GraphQL.Draft.Printer
import Language.GraphQL.Draft.Syntax
import Test.Tasty.Bench
import Text.Builder qualified as TB

genDocs :: Int -> IO [(Int, ExecutableDocument Name)]
genDocs num =
  forM [1 .. num] $ \n -> (n,) <$> generate genExecutableDocument

genTexts :: Int -> IO [(Int, [T.Text])]
genTexts num =
  forM [1 .. num] $ \n -> do
    texts <- forM [1 .. 500] $ const $ generate genText
    pure $ (n, texts)

main :: IO ()
main = do
  docs <- genDocs 10
  texts <- genTexts 10
  let grp1 = mkPPGrp docs
      grp2 = mkBBGrp docs
      grp3 = mkTBGrp docs
      grp4 = mkTLBGrp docs
      renderedDocs = map (\(n, q) -> (n, renderExecutableDoc q)) docs
      grp5 = mkPGrp renderedDocs
      grp6 = mkNGrp texts
  defaultMain [grp1, grp2, grp3, grp4, grp5, grp6]
  where
    mkNGrp texts =
      bgroup "checking name validity" $
        texts <&> \(n, t) ->
          bench (show n) $ nf (length . catMaybes . map mkName) t

    mkPGrp qs =
      bgroup "parsing executableDocument" $
        map (\(n, q) -> bench (show n) $ whnf parseExecutableDoc q) qs

    mkPPGrp gqs =
      bgroup "rendering executableDocument (prettyprinter)" $
        map (\(n, gq) -> bench (show n) $ nf (renderPP . executableDocument) gq) gqs

    mkBBGrp gqs =
      bgroup "rendering executableDocument (bytestring builder)" $
        map (\(n, gq) -> bench (show n) $ nf (renderBB . executableDocument) gq) gqs

    mkTBGrp gqs =
      bgroup "rendering executableDocument (text builder)" $
        map (\(n, gq) -> bench (show n) $ nf (renderTB . executableDocument) gq) gqs

    mkTLBGrp gqs =
      bgroup "rendering executableDocument (lazy text builder)" $
        map (\(n, gq) -> bench (show n) $ nf (renderTLB . executableDocument) gq) gqs

    renderPP :: PP.Doc T.Text -> T.Text
    renderPP = PP.renderStrict . PP.layoutPretty PP.defaultLayoutOptions
    renderBB = BS.toLazyByteString
    renderTB = TB.run
    renderTLB = TL.toLazyText
