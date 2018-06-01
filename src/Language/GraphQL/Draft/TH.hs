module Language.GraphQL.Draft.TH
  ( parseSchemaDocQ
  , parseExecutableDocQ
  ) where

import           Control.Monad.Fail            (fail)
import           Protolude
import           System.FilePath               (FilePath)

import qualified Data.Text.IO                  as TI
import qualified Language.Haskell.TH.Syntax    as TH

import           Language.GraphQL.Draft.Parser

parseSchemaDocQ :: FilePath -> TH.Q TH.Exp
parseSchemaDocQ path = do
  TH.addDependentFile path
  content <- TH.runIO $ TI.readFile path
  case parseSchemaDoc content of
    Left err -> fail $ toS err
    Right x  -> TH.lift x

parseExecutableDocQ :: FilePath -> TH.Q TH.Exp
parseExecutableDocQ path = do
  TH.addDependentFile path
  content <- TH.runIO $ TI.readFile path
  case parseExecutableDoc content of
    Left err -> fail $ toS err
    Right x  -> TH.lift x
