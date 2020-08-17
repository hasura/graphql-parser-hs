module Language.GraphQL.Draft.Generator where

import           Control.Monad.IO.Class
import           Data.Maybe                    (fromJust)
import           Data.Scientific               (fromFloatDigits)
import           Data.Text                     (Text)
import           Data.Void
import           Hedgehog

import qualified Data.HashMap.Strict           as M
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

import           Language.GraphQL.Draft.Syntax



-- | *Document*

generate :: MonadIO m => Gen a -> m a
generate = Gen.sample

genDocument :: Gen Document
genDocument =
  Document <$> Gen.list (Range.linear 1 3) genDefinition

genExecutableDocument :: Gen (ExecutableDocument Name)
genExecutableDocument =
  ExecutableDocument <$> Gen.list (Range.linear 1 3) (genExecutableDefinition genValue)

genVoidExecutableDocument :: Gen (ExecutableDocument Void)
genVoidExecutableDocument =
  ExecutableDocument <$> Gen.list (Range.linear 1 3) (genExecutableDefinition genDefaultValue)

genSchemaDocument :: Gen SchemaDocument
genSchemaDocument =
  SchemaDocument <$> Gen.list (Range.linear 1 5) genTypeDefinition



-- | *Identifiers*

genText :: Gen Text
genText = Gen.text (Range.linear 1 11) Gen.unicode

genGraphqlName :: Gen Text
genGraphqlName = Gen.text (Range.singleton 1) Gen.alpha <>
                 Gen.text (Range.linear 1 11) Gen.alphaNum

genName :: Gen Name
genName = fromJust . mkName <$> genGraphqlName

genNullability :: Gen Nullability
genNullability = Nullability <$> Gen.bool

genType :: Gen GType
genType =
  Gen.recursive
  Gen.choice
  [TypeNamed <$> genNullability <*> genName]
  [TypeList  <$> genNullability <*> genType]

genDescription :: Gen Description
genDescription = Description <$> genText



-- | *Values*

genValue :: Gen (Value Name)
genValue =
  -- TODO: use maxbound of int32/double or something?
  Gen.recursive
  Gen.choice
  [ pure VNull
  , VInt <$> fromIntegral <$> Gen.int32 (Range.linear 1 99999)
  , VEnum <$> genEnumValue
  , VFloat <$> fromFloatDigits <$> Gen.double (Range.linearFrac 1.1 999999.99999)
  , VString <$> genText
  , VBoolean <$> Gen.bool
  , VVariable <$> genName
  ]
  [ VList <$> (genListValue genValue)
  , VObject <$> (genObjectValue genValue)
  ]

genDefaultValue :: Gen (Value Void)
genDefaultValue =
  -- TODO: use maxbound of int32/double or something?
  Gen.recursive
  Gen.choice
  [ pure VNull
  , VInt <$> fromIntegral <$> Gen.int32 (Range.linear 1 99999)
  , VEnum <$> genEnumValue
  , VFloat <$> fromFloatDigits <$> Gen.double (Range.linearFrac 1.1 999999.99999)
  , VString <$> genText
  , VBoolean <$> Gen.bool
  ]
  [ VList <$> (genListValue genDefaultValue)
  , VObject <$> (genObjectValue genDefaultValue)
  ]

genEnumValue :: Gen EnumValue
genEnumValue = EnumValue <$> genName

genListValue :: Gen (Value a) -> Gen [Value a]
genListValue genVal = Gen.list (Range.linear 1 4) genVal

genObjectValue :: Gen (Value a) -> Gen (M.HashMap Name (Value a))
genObjectValue genVal = M.fromList <$> mkList genObjectField
  where
    genObjectField = (,) <$> genName <*> genVal



-- | *Definitions*

genDefinition :: Gen Definition
genDefinition =
  Gen.choice [ DefinitionExecutable <$> genExecutableDefinition genValue
             , DefinitionTypeSystem <$> genTypeSystemDefinition
             ]

genExecutableDefinition :: Gen (Value a) -> Gen (ExecutableDefinition a)
genExecutableDefinition genVal =
  Gen.choice [ ExecutableDefinitionOperation <$> genOperationDefinition genVal
             , ExecutableDefinitionFragment <$> genFragmentDefinition
             ]

genOperationDefinition :: Gen (Value a) -> Gen (OperationDefinition FragmentSpread a)
genOperationDefinition genVal =
  Gen.choice [ OperationDefinitionTyped <$> genTypedOperationDefinition genVal
             , OperationDefinitionUnTyped <$> genSelectionSet genVal
             ]


genTypedOperationDefinition :: Gen (Value a) -> Gen (TypedOperationDefinition FragmentSpread a)
genTypedOperationDefinition genVal =
  TypedOperationDefinition
  <$> genOperationType
  <*> Gen.maybe genName
  <*> mkList genVariableDefinition
  <*> genDirectives genVal
  <*> genSelectionSet genVal

genVariableDefinition :: Gen VariableDefinition
genVariableDefinition = VariableDefinition
                        <$> genName
                        <*> genType
                        <*> Gen.maybe genDefaultValue

genFragmentDefinition :: Gen FragmentDefinition
genFragmentDefinition = FragmentDefinition
                        <$> genName
                        <*> genName
                        <*> genDirectives genDefaultValue
                        <*> genSelectionSet genDefaultValue

genTypeSystemDefinition :: Gen TypeSystemDefinition
genTypeSystemDefinition =
  Gen.choice [ TypeSystemDefinitionSchema <$> genSchemaDefinition
             , TypeSystemDefinitionType <$> genTypeDefinition
             ]

genSchemaDefinition :: Gen SchemaDefinition
genSchemaDefinition = SchemaDefinition
                      <$> Gen.maybe (genDirectives genDefaultValue)
                      <*> mkList genRootOperationTypeDefinition

genRootOperationTypeDefinition :: Gen RootOperationTypeDefinition
genRootOperationTypeDefinition = RootOperationTypeDefinition
                                 <$> genOperationType
                                 <*> genName

genOperationType :: Gen OperationType
genOperationType =
  Gen.element [ OperationTypeQuery
              , OperationTypeMutation
              , OperationTypeSubscription
              ]

genTypeDefinition :: Gen (TypeDefinition ())
genTypeDefinition =
  Gen.choice [ TypeDefinitionScalar      <$> genScalarTypeDefinition
             , TypeDefinitionObject      <$> genObjectTypeDefinition
             , TypeDefinitionInterface   <$> genInterfaceTypeDefinition
             , TypeDefinitionUnion       <$> genUnionTypeDefinition
             , TypeDefinitionEnum        <$> genEnumTypeDefinition
             , TypeDefinitionInputObject <$> genInputObjectTypeDefinition
             ]

genScalarTypeDefinition :: Gen ScalarTypeDefinition
genScalarTypeDefinition = ScalarTypeDefinition
                          <$> Gen.maybe genDescription
                          <*> genName
                          <*> genDirectives genDefaultValue

genObjectTypeDefinition :: Gen ObjectTypeDefinition
genObjectTypeDefinition = ObjectTypeDefinition
                          <$> Gen.maybe genDescription
                          <*> genName
                          <*> mkList genName
                          <*> genDirectives genDefaultValue
                          <*> genFieldDefinitions

genInterfaceTypeDefinition :: Gen (InterfaceTypeDefinition ())
genInterfaceTypeDefinition = InterfaceTypeDefinition
                             <$> Gen.maybe genDescription
                             <*> genName
                             <*> genDirectives genDefaultValue
                             <*> genFieldDefinitions
                             <*> pure ()

genUnionTypeDefinition :: Gen UnionTypeDefinition
genUnionTypeDefinition = UnionTypeDefinition
                         <$> Gen.maybe genDescription
                         <*> genName
                         <*> genDirectives genDefaultValue
                         <*> mkList genName

genEnumTypeDefinition :: Gen EnumTypeDefinition
genEnumTypeDefinition = EnumTypeDefinition
                        <$> Gen.maybe genDescription
                        <*> genName
                        <*> genDirectives genDefaultValue
                        <*> mkList genEnumValueDefinition

genInputObjectTypeDefinition :: Gen InputObjectTypeDefinition
genInputObjectTypeDefinition = InputObjectTypeDefinition
                               <$> Gen.maybe genDescription
                               <*> genName
                               <*> genDirectives genDefaultValue
                               <*> mkList genInputValueDefinition

genInputValueDefinition :: Gen InputValueDefinition
genInputValueDefinition = InputValueDefinition
                          <$> Gen.maybe genDescription
                          <*> genName
                          <*> genType
                          <*> Gen.maybe genDefaultValue

genEnumValueDefinition :: Gen EnumValueDefinition
genEnumValueDefinition = EnumValueDefinition
                         <$> Gen.maybe genDescription
                         <*> genEnumValue
                         <*> genDirectives genDefaultValue

genFieldDefinition :: Gen FieldDefinition
genFieldDefinition = FieldDefinition
                     <$> Gen.maybe genDescription
                     <*> genName
                     <*> mkList genInputValueDefinition
                     <*> genType
                     <*> genDirectives genDefaultValue

genFieldDefinitions :: Gen [FieldDefinition]
genFieldDefinitions = mkList genFieldDefinition


genDirectiveDefinition :: Gen DirectiveDefinition
genDirectiveDefinition = DirectiveDefinition
                         <$> Gen.maybe genDescription
                         <*> genName
                         <*> genArgumentsDefinition
                         <*> Gen.list (Range.linear 1 10) genDirectiveLocation


genArgumentsDefinition :: Gen ArgumentsDefinition
genArgumentsDefinition = Gen.list (Range.linear 1 10) genInputValueDefinition

genDirectiveLocation :: Gen DirectiveLocation
genDirectiveLocation =
  Gen.choice [ DLExecutable <$> genExecutableDirectiveLocation
             , DLTypeSystem <$> genTypeSystemDirectiveLocation
             ]

genExecutableDirectiveLocation :: Gen ExecutableDirectiveLocation
genExecutableDirectiveLocation =
  Gen.element [ EDLQUERY
              , EDLMUTATION
              , EDLSUBSCRIPTION
              , EDLFIELD
              , EDLFRAGMENT_DEFINITION
              , EDLFRAGMENT_SPREAD
              , EDLINLINE_FRAGMENT
              ]

genTypeSystemDirectiveLocation :: Gen TypeSystemDirectiveLocation
genTypeSystemDirectiveLocation =
  Gen.element [ TSDLSCHEMA
              , TSDLSCALAR
              , TSDLOBJECT
              , TSDLFIELD_DEFINITION
              , TSDLARGUMENT_DEFINITION
              , TSDLINTERFACE
              , TSDLUNION
              , TSDLENUM
              , TSDLENUM_VALUE
              , TSDLINPUT_OBJECT
              , TSDLINPUT_FIELD_DEFINITION
             ]

genSelectionSet :: Gen (Value a) -> Gen (SelectionSet FragmentSpread a)
genSelectionSet genVal = mkList $ genSelection genVal

genSelection :: Gen (Value a) -> Gen (Selection FragmentSpread a)
genSelection genVal =
  Gen.recursive
  Gen.choice [ SelectionFragmentSpread <$> genFragmentSpread genVal
             ]
             [ SelectionField          <$> genField genVal
             , SelectionInlineFragment <$> genInlineFragment genVal
             ]

genFragmentSpread :: Gen (Value a) -> Gen (FragmentSpread a)
genFragmentSpread genVal = FragmentSpread
                           <$> genName
                           <*> genDirectives genVal

genInlineFragment :: Gen (Value a) -> Gen (InlineFragment FragmentSpread a)
genInlineFragment genVal = InlineFragment
                           <$> Gen.maybe genName
                           <*> genDirectives genVal
                           <*> genSelectionSet genVal

genField :: Gen (Value a) -> Gen (Field FragmentSpread a)
genField genVal = Field
                  <$> Gen.maybe genName
                  <*> genName
                  <*> (M.fromList <$> mkList (genArgument genVal))
                  <*> genDirectives genVal
                  <*> genSelectionSet genVal

genDirective :: Gen (Value a) -> Gen (Directive a)
genDirective genVal = Directive
                      <$> genName
                      <*> (M.fromList <$> mkList (genArgument genVal))

genDirectives :: Gen (Value a) -> Gen [Directive a]
genDirectives = mkList . genDirective

genArgument :: Gen (Value a) -> Gen (Name, Value a)
genArgument genVal = (,) <$> genName <*> genVal



-- | *Definitions*

mkList :: Gen a -> Gen [a]
mkList = Gen.list $ Range.linear 1 11
