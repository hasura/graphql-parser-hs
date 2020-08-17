import           Control.Monad
import           Criterion.Main

import qualified Data.ByteString.Builder               as BS
import qualified Data.Text                             as T
import qualified Data.Text.Lazy.Builder                as TL
import qualified Data.Text.Prettyprint.Doc             as PP
import qualified Data.Text.Prettyprint.Doc.Render.Text as PP
import qualified Text.Builder                          as TB

import           Language.GraphQL.Draft.Generator
import           Language.GraphQL.Draft.Parser         (parseExecutableDoc)
import           Language.GraphQL.Draft.Printer
import           Language.GraphQL.Draft.Syntax


genDocs :: Int -> IO [(Int, ExecutableDocument Name)]
genDocs num =
  forM [1..num] $ \n -> (,) <$> pure n <*> generate genExecutableDocument

main :: IO ()
main = do
  docs <- genDocs 10
  let grp1 = mkPPGrp docs
      grp2 = mkBBGrp docs
      grp3 = mkTBGrp docs
      grp4 = mkTLBGrp docs
      renderedDocs = map (\(n, q) -> (n, renderExecutableDoc q)) docs
      grp5 = mkPGrp renderedDocs
  defaultMain [grp1, grp2, grp3, grp4, grp5]
  where
    mkPGrp qs =
      bgroup "parsing executableDocument" $
      map (\(n, q) -> bench (show n) $ whnf parseExecutableDoc q) qs

    mkPPGrp gqs =
      bgroup "rendering executableDocument (prettyprinter)" $
      map (\(n, gq) -> bench (show n) $ nf (renderPP . executableDocument) gq) gqs

    mkBBGrp gqs = bgroup "rendering executableDocument (bytestring builder)" $
      map (\(n, gq) -> bench (show n) $ nf (renderBB . executableDocument) gq) gqs

    mkTBGrp gqs = bgroup "rendering executableDocument (text builder)" $
      map (\(n, gq) -> bench (show n) $ nf (renderTB . executableDocument) gq) gqs

    mkTLBGrp gqs = bgroup "rendering executableDocument (lazy text builder)" $
      map (\(n, gq) -> bench (show n) $ nf (renderTLB . executableDocument) gq) gqs

    renderPP :: PP.Doc T.Text -> T.Text
    renderPP  = PP.renderStrict . PP.layoutPretty PP.defaultLayoutOptions
    renderBB  = BS.toLazyByteString
    renderTB  = TB.run
    renderTLB = TL.toLazyText
