{-# LANGUAGE OverloadedStrings #-}


import Protolude
import Language.GraphQL.Draft.Parser
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.IORef
import System.IO.Error
import UnitTests
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  string <- getArgs >>= maybe (return "") T.readFile . head
  let cur = parseExecutableDoc string
  print cur
  testsVal <- testSpec "Parsing Values" values
  testsUntyped <- testSpec "Parsing Untyped Queries" untypedQueries
  testsTyped  <- testSpec "Parsing Typed Queries" typedQueries
  defaultMain $ testGroup "GraphQL" [testsVal, testsUntyped, testsTyped]
