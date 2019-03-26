import           Criterion.Main
import           Protolude

import           Generator.Language.GraphQL.Document
import           Language.GraphQL.Draft.Parser             (parseExecutableDoc)
import           Language.GraphQL.Draft.Syntax

import qualified Language.GraphQL.Draft.Printer.ByteString as PP.BB
import qualified Language.GraphQL.Draft.Printer.Pretty     as PP
import qualified Language.GraphQL.Draft.Printer.Text       as PP.TB


genDocs :: Int -> IO [(Int, ExecutableDocument)]
genDocs num =
  forM [1..num] $ \n -> (,) <$> pure n <*> generate genExecutableDocument

renderExeDocC :: ExecutableDocument -> Text
renderExeDocC = PP.renderCompact . PP.executableDocument

renderExeDoc :: ExecutableDocument -> Text
renderExeDoc = PP.renderPretty . PP.executableDocument

main :: IO ()
main = do
  docs <- genDocs 10
  let grp1 = mkPPGrp docs
      grp2 = mkBBGrp docs
      grp3 = mkTBGrp docs
      renderedDocs = map (\(n, q) -> (n, renderExeDocC q)) docs
      grp4 = mkPGrp renderedDocs
  defaultMain [grp1, grp2, grp3, grp4]
  where
    mkPGrp qs =
      bgroup "parsing executableDocument" $
      map (\(n, q) -> bench (show n) $ whnf parseExecutableDoc q) qs

    mkPPGrp gqs =
      bgroup "rendering executableDocument (prettyprinter)" $
      map (\(n, gq) -> bench (show n) $ nf renderExeDocC gq) gqs

    mkBBGrp gqs = bgroup "rendering executableDocument (bytestring builder)" $
      map (\(n, gq) -> bench (show n) $ nf PP.BB.renderExecutableDoc gq) gqs

    mkTBGrp gqs = bgroup "rendering executableDocument (text builder)" $
      map (\(n, gq) -> bench (show n) $ nf PP.TB.renderExecutableDoc gq) gqs
