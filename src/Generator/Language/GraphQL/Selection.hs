module Generator.Language.GraphQL.Selection where

import           Hedgehog
import           Protolude

import qualified Hedgehog.Gen                          as Gen
import qualified Hedgehog.Range                        as Range

import           Generator.Language.GraphQL.Primitives
import           Language.GraphQL.Draft.Syntax


genSelectionSet :: Gen SelectionSet
genSelectionSet = Gen.list (Range.linear 1 11) genSelection

genSelection :: Gen Selection
genSelection =
  Gen.recursive
  Gen.choice [ SelectionFragmentSpread <$> genFragmentSpread
             ]
             [ Gen.subtermM (SelectionField <$> genField) pure
             , Gen.subtermM (SelectionInlineFragment <$> genInlineFragment) pure
             ]

genFragmentSpread :: Gen FragmentSpread
genFragmentSpread = FragmentSpread
                    <$> genName
                    <*> genDirectives

genInlineFragment :: Gen InlineFragment
genInlineFragment = InlineFragment
                    <$> Gen.maybe genTypeCondition
                    <*> genDirectives
                    <*> genSelectionSet

genField :: Gen Field
genField = Field
           <$> Gen.maybe genAlias
           <*> genName
           <*> Gen.list (Range.linear 1 11) genArgument
           <*> genDirectives
           <*> genSelectionSet

genAlias :: Gen Alias
genAlias = Alias <$> genName

genDirective :: Gen Directive
genDirective = Directive
               <$> genName
               <*> Gen.list (Range.linear 1 11) genArgument

genDirectives :: Gen [Directive]
genDirectives = Gen.list (Range.linear 1 11) genDirective

genArgument :: Gen Argument
genArgument = Argument <$> genName <*> genValue

genTypeCondition :: Gen TypeCondition
genTypeCondition = genNamedType
