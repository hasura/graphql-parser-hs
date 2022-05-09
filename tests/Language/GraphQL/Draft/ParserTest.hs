{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Language.GraphQL.Draft.ParserTest
  ( hprop_test,
  )
where

-------------------------------------------------------------------------------

import Hedgehog (Property, property, (===))
import Prelude

-------------------------------------------------------------------------------

hprop_test :: Property
hprop_test = property $ 2 === (3 :: Int)
