module Language.GraphQL.Draft.Generator.Primitives where

import           Hedgehog
import           Protolude

import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

import           Data.Scientific (fromFloatDigits)

import           Language.GraphQL.Draft.Syntax


-- | *Identifiers*

genText :: Gen Text
genText = Gen.text (Range.linear 1 11) Gen.unicode

genGraphqlName :: Gen Text
genGraphqlName = Gen.text (Range.singleton 1) Gen.alpha <>
                 Gen.text (Range.linear 1 11) Gen.alphaNum

genName :: Gen Name
genName = Name <$> genGraphqlName

genNamedType :: Gen NamedType
genNamedType = NamedType <$> genName

genDescription :: Gen Description
genDescription = Description <$> genText


-- | *Values*

genValue :: Gen Value
genValue =
  -- TODO: use maxbound of int32/double or something?
  Gen.recursive
  Gen.choice [ pure VNull
             , VScientific <$> fromIntegral <$> Gen.int32 (Range.linear 1 99999)
             , VEnum <$> genEnumValue
             , VScientific <$> fromFloatDigits <$> Gen.double (Range.linearFrac 1.1 999999.99999)
             , VString <$> genStringValue
             , VBoolean <$> Gen.bool
             , VVariable <$> genVariable
             ]
             [ Gen.subtermM (VList <$> genListValue) pure
             , Gen.subtermM (VObject <$> genObjectValue) pure
             ]

genStringValue :: Gen StringValue
genStringValue = StringValue . unName <$> genName

genVariable :: Gen Variable
genVariable = Variable <$> genName

genEnumValue :: Gen EnumValue
genEnumValue = EnumValue <$> genName

genListValue :: Gen ListValue
genListValue = ListValueG <$> Gen.list (Range.linear 1 11) genValue

genObjectValue :: Gen ObjectValue
genObjectValue = ObjectValueG <$> Gen.list (Range.linear 1 11) genObjectField

genObjectField :: Gen ObjectField
genObjectField = ObjectFieldG <$> genName <*> genValue

genDefaultValue :: Gen DefaultValue
genDefaultValue = genValueConst

genValueConst :: Gen ValueConst
genValueConst =
  -- TODO: use maxbound of int64/double or something?
  Gen.recursive
  Gen.choice [ pure VCNull
             , VCScientific <$> fromIntegral <$> Gen.int32 (Range.linear 1 9)
             , VCEnum <$> genEnumValue
             , VCScientific <$> fromFloatDigits <$> Gen.double (Range.linearFrac 1.1 999999.99999)
             , VCString <$> genStringValue
             , VCBoolean <$> Gen.bool
             ]
             [ Gen.subtermM (VCList <$> genListValueC) pure
             , Gen.subtermM (VCObject <$> genObjectValueC) pure
             ]

genListValueC :: Gen ListValueC
genListValueC = ListValueG <$> Gen.list (Range.linear 1 11) genValueConst

genObjectValueC :: Gen ObjectValueC
genObjectValueC = ObjectValueG <$> Gen.list (Range.linear 1 11) genObjectFieldC

genObjectFieldC :: Gen ObjectFieldC
genObjectFieldC = ObjectFieldG <$> genName <*> genValueConst
