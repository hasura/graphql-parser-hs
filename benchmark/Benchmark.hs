import           Criterion.Main
import           Protolude

import           Generator.Language.GraphQL.Document
import           Language.GraphQL.Draft.Parser             (parseExecutableDoc)
import           Language.GraphQL.Draft.Syntax

import qualified Language.GraphQL.Draft.Printer.ByteString as PP.BB
import qualified Language.GraphQL.Draft.Printer.Pretty     as PP


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
      renderedDocs = map (\(n, q) -> (n, renderExeDocC q)) docs
      grp3 = mkPGrp renderedDocs
  defaultMain [grp1, grp2, grp3]
  where
    mkPGrp qs =
      bgroup "parsing executableDocument" $
      map (\(n, q) -> bench (show n) $ whnf parseExecutableDoc q) qs

    mkPPGrp gqs =
      bgroup "rendering executableDocument (prettyprinter)" $
      map (\(n, gq) -> bench (show n) $ whnf renderExeDocC gq) gqs

    mkBBGrp gqs = bgroup "rendering executableDocument (bytestring builder)" $
      map (\(n, gq) -> bench (show n) $ whnf PP.BB.renderExecutableDoc gq) gqs
