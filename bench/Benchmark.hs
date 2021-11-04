import           Control.Monad
import           Criterion.Main
import           Data.Functor                          ((<&>))
import           Data.Maybe                            (catMaybes)

import qualified Data.ByteString.Builder               as BS
import qualified Data.Text                             as T
import qualified Data.Text.Lazy.Builder                as TL
import qualified Prettyprinter                         as PP
import qualified Data.Text.Prettyprint.Doc.Render.Text as PP
import qualified Text.Builder                          as TB

import           Language.GraphQL.Draft.Generator
import           Language.GraphQL.Draft.Parser         (parseExecutableDoc)
import           Language.GraphQL.Draft.Printer
import           Language.GraphQL.Draft.Syntax


genDocs :: Int -> IO [(Int, ExecutableDocument Name)]
genDocs num =
  forM [1..num] $ \n -> (n,) <$> generate genExecutableDocument

genTexts :: Int -> IO [(Int, [T.Text])]
genTexts num =
  forM [1..num] $ \n -> do
    texts <- forM [(1::Int)..500] $ const $ generate genText
    pure $ (n, texts)

main :: IO ()
main = do
  docs  <- genDocs 10
  texts <- genTexts 10
  -- FIXME: supply a fixed seed to hedgehog; currently these benchmark random
  -- payloads every execution so the numbers aren't very meaningful AFAICT
  -- Adapt: https://hackage.haskell.org/package/hedgehog-1.0.5/docs/src/Hedgehog.Internal.Gen.html#sample 
  let grp1 = mkPPGrp docs
      grp2 = mkBBGrp docs
      grp3 = mkTBGrp docs
      grp4 = mkTLBGrp docs
      renderedDocs = map (\(n, q) -> (n, renderExecutableDoc q)) docs
      grp5 = mkPGrp renderedDocs
      grp6 = mkNGrp texts
  -- ...these ones are stable though; these queries are taken from the hasura
  -- server benchmark suite:
  let grpStableParse = bgroup "stable parseExecutableDoc" [
          bench "chinook simple_query"   $ nf parseExecutableDoc queryChinookSimple
        , bench "chinook complex_query"  $ nf parseExecutableDoc queryChinookComplex
        , bench "huge_schema huge_query" $ nf parseExecutableDoc queryHugeSchemaHuge
        ]
  defaultMain [grp1, grp2, grp3, grp4, grp5, grp6, grpStableParse]
  where
    mkNGrp texts =
      bgroup "checking name validity" $ texts <&> \(n,t) ->
        bench (show n) $ nf (length . catMaybes . map mkName) t

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

-- ----------------------------------------------------------------------------
-- These queries are copied verbatim from
--   https://github.com/hasura/graphql-engine-mono/tree/main/server/benchmarks
-- ...as realistic queries at small/medium/large sizes, and so that we can
-- directly compare the microbenchmarks here to latency numbers reported from
-- the benchmarks suite:

queryChinookSimple :: T.Text
queryChinookSimple = T.pack "\
     \query MyQuery {\n\
     \  Customer {\n\
     \    Email\n\
     \  }\n\
     \}\n\
     \"


queryChinookComplex :: T.Text
queryChinookComplex = T.pack "\
     \query MyQuery($genre: String!, $track_lim: Int = 1000) {\n\
     \  playlist_containing_genre: Playlist(order_by: {Name: asc}, where: {PlaylistTracks: {Track: {Genre: {Name: {_eq: $genre}}}}}) {\n\
     \    Name\n\
     \    tracks_of_genre: PlaylistTracks(where: {Track: {Genre: {Name: {_eq: $genre}}}}, limit: $track_lim) {\n\
     \      Track {\n\
     \        Name\n\
     \        Album {\n\
     \          Title\n\
     \          Artist {\n\
     \            Name\n\
     \          }\n\
     \        }\n\
     \        MediaType {\n\
     \          Name\n\
     \        }\n\
     \      }\n\
     \    }\n\
     \  }\n\
     \}\n\
     \"


queryHugeSchemaHuge :: T.Text
queryHugeSchemaHuge = T.pack "\
     \query MyQuery {\n\
     \  avnnjybkglhndgc {\n\
     \    bhdvbvtikfpzzzi {\n\
     \      amwqazmmnrmnpep\n\
     \      avnnjybkglhndgc(limit: 2) {\n\
     \        bhdvbvtikfpzzzi {\n\
     \          amwqazmmnrmnpep\n\
     \          aelitqplyjbudtg\n\
     \          avnnjybkglhndgc_aggregate {\n\
     \            aggregate {\n\
     \              count\n\
     \            }\n\
     \          }\n\
     \          avnnjybkglhndgc {\n\
     \            bhdvbvtikfpzzzi {\n\
     \              avnnjybkglhndgc {\n\
     \                ypyynrnicfelhoa\n\
     \                mozxpgxfqyoflfy\n\
     \                ftbuftsifulkfef\n\
     \                gcgxpbzitewgjfq(where: {_and: {avnnjybkglhndgc: {bhdvbvtikfpzzzi: {avnnjybkglhndgc: {ypyynrnicfelhoa: {_eq: \"\"}}}}}}) {\n\
     \                  bgssgjcrnyvdhgj\n\
     \                  bzvpaxbebukffqa\n\
     \                  felhxylqzathmzg\n\
     \                  jiwnvptjddwkpub\n\
     \                  kwuvazmzrckjiwd\n\
     \                  lfbdaknrdgrdyob\n\
     \                  lsdbuqrfcmzuoiq\n\
     \                  ppqfbdpbvygbjqe\n\
     \                  pyrmluepmvbkauy\n\
     \                  rjzfajqvaxfjnfv\n\
     \                  tkxykgrteozwppc\n\
     \                  yppheggqiyclbmg\n\
     \                }\n\
     \                ytbguypxmteukch {\n\
     \                  eioyhdrhnxvnjgq\n\
     \                  chavxisgckppceo\n\
     \                  emojtgbykwcsasf_aggregate(limit: 1) {\n\
     \                    nodes {\n\
     \                      atujakdwuavibfs\n\
     \                      mozxpgxfqyoflfy\n\
     \                      ytbguypxmteukch {\n\
     \                        azyplftmeorxhbg {\n\
     \                          uclgdxfykcsupfy(where: {zchccmznhdwcyyk: {_gt: 10}}) {\n\
     \                            fbrjqdhyhfrufka\n\
     \                            fdaufkdgffxhwjt\n\
     \                            eresulfxruxgauy\n\
     \                          }\n\
     \                        }\n\
     \                      }\n\
     \                    }\n\
     \                  }\n\
     \                }\n\
     \              }\n\
     \            }\n\
     \          }\n\
     \          rgajytdbzlrqtjc\n\
     \          qwfxccufmuuhdkh\n\
     \          qiirajrbqfhtepv {\n\
     \            byuwtzavnnfphdb\n\
     \            cfceikbyshpbgzb_aggregate(offset: 10) {\n\
     \              nodes {\n\
     \                eydctogzeddjeje\n\
     \                dtxtdmrvbiqdnhw\n\
     \                fzbriqaymwtbbcv(limit: 2) {\n\
     \                  atujakdwuavibfs\n\
     \                  mozxpgxfqyoflfy\n\
     \                  mwcitdymkwgnljf\n\
     \                  ypyynrnicfelhoa\n\
     \                }\n\
     \              }\n\
     \            }\n\
     \          }\n\
     \        }\n\
     \      }\n\
     \    }\n\
     \  }\n\
     \  rvkwxbcbykpolag(order_by: {cuyspsadpcpwfgn: asc}) {\n\
     \    cuyspsadpcpwfgn\n\
     \    gwkbipliejeqvck\n\
     \    ivxowxfitlpzwma\n\
     \    hjyfezsxxymocek {\n\
     \      ajmryhdgplutcfv\n\
     \      ankyuplezhaxmwh\n\
     \    }\n\
     \  }\n\
     \  ylfijjhnpbqnxdg {\n\
     \    qqhruyfclqsgkoi\n\
     \    rghujqiztetxqvr\n\
     \    rmiwettrtghoqrg {\n\
     \      brtgvqwxpxxpjsn(distinct_on: qqhruyfclqsgkoi) {\n\
     \        qqhruyfclqsgkoi\n\
     \      }\n\
     \      dhbepsvydbaowdg(where: {hhzbfpqbvtemdxn: {awzuxwwdvulkzsp: {_gt: 10}}}) {\n\
     \        qqhruyfclqsgkoi\n\
     \        rghujqiztetxqvr\n\
     \      }\n\
     \    }\n\
     \  }\n\
     \  znxjapvcubkzhtv_aggregate {\n\
     \    aggregate {\n\
     \      count(distinct: false)\n\
     \    }\n\
     \    nodes {\n\
     \      dodppghpbvtdrzt\n\
     \      dovdwfgspicingr\n\
     \      dczgjksdfomxthp\n\
     \    }\n\
     \  }\n\
     \}\n\
     \"
