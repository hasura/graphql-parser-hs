module Language.GraphQL.Draft.Generator.TypeDefinition where

import           Hedgehog
import           Protolude

import qualified Hedgehog.Gen                                as Gen
import qualified Hedgehog.Range                              as Range

import           Language.GraphQL.Draft.Generator.Primitives
import           Language.GraphQL.Draft.Generator.Selection
import           Language.GraphQL.Draft.Syntax


genDefinition :: Gen Definition
genDefinition =
  Gen.choice [ DefinitionExecutable <$> genExecutableDefinition
             , DefinitionTypeSystem <$> genTypeSystemDefinition
             ]

genExecutableDefinition :: Gen ExecutableDefinition
genExecutableDefinition =
  Gen.choice [ ExecutableDefinitionOperation <$> genOperationDefinition
             , ExecutableDefinitionFragment <$> genFragmentDefinition
             ]

genOperationDefinition :: Gen OperationDefinition
genOperationDefinition =
  Gen.choice [ OperationDefinitionTyped <$> genTypedOperationDefinition
             , OperationDefinitionUnTyped <$> genSelectionSet
             ]


genTypedOperationDefinition :: Gen TypedOperationDefinition
genTypedOperationDefinition = TypedOperationDefinition
                              <$> genOperationType
                              <*> Gen.maybe genName
                              <*> Gen.list (Range.linear 1 11) genVariableDefinition
                              <*> genDirectives
                              <*> genSelectionSet

genVariableDefinition :: Gen VariableDefinition
genVariableDefinition = VariableDefinition
                        <$> genVariable
                        <*> genGType
                        <*> Gen.maybe genDefaultValue

genFragmentDefinition :: Gen FragmentDefinition
genFragmentDefinition = FragmentDefinition
                        <$> genName
                        <*> genTypeCondition
                        <*> genDirectives
                        <*> genSelectionSet

genTypeSystemDefinition :: Gen TypeSystemDefinition
genTypeSystemDefinition =
  Gen.choice [ TypeSystemDefinitionSchema <$> genSchemaDefinition
             , TypeSystemDefinitionType <$> genTypeDefinition
             ]

genSchemaDefinition :: Gen SchemaDefinition
genSchemaDefinition = SchemaDefinition
                      <$> Gen.maybe genDirectives
                      <*> Gen.list (Range.linear 1 11) genRootOperationTypeDefinition

genRootOperationTypeDefinition :: Gen RootOperationTypeDefinition
genRootOperationTypeDefinition = RootOperationTypeDefinition
                                 <$> genOperationType
                                 <*> genNamedType

genOperationType :: Gen OperationType
genOperationType =
  Gen.element [ OperationTypeQuery
              , OperationTypeMutation
              , OperationTypeSubscription
              ]

genTypeDefinition :: Gen TypeDefinition
genTypeDefinition =
  Gen.choice [ TypeDefinitionScalar <$> genScalarTypeDefinition
             , TypeDefinitionObject <$> genObjectTypeDefinition
             , TypeDefinitionInterface <$> genInterfaceTypeDefinition
             , TypeDefinitionUnion <$> genUnionTypeDefinition
             , TypeDefinitionEnum <$> genEnumTypeDefinition
             , TypeDefinitionInputObject <$> genInputObjectTypeDefinition
             ]

genNullability :: Gen Nullability
genNullability = Nullability <$> Gen.bool

genListType :: Gen ListType
genListType = ListType <$> genGType

genGType :: Gen GType
genGType =
  Gen.choice [ TypeNamed <$> genNullability <*> genNamedType
             , TypeList <$> genNullability <*> genListType
             ]

genScalarTypeDefinition :: Gen ScalarTypeDefinition
genScalarTypeDefinition = ScalarTypeDefinition
                          <$> Gen.maybe genDescription
                          <*> genName
                          <*> genDirectives

genObjectTypeDefinition :: Gen ObjectTypeDefinition
genObjectTypeDefinition = ObjectTypeDefinition
                          <$> Gen.maybe genDescription
                          <*> genName
                          <*> Gen.list (Range.linear 1 11) genNamedType
                          <*> genDirectives
                          <*> genFieldDefinitions

genInterfaceTypeDefinition :: Gen InterfaceTypeDefinition
genInterfaceTypeDefinition = InterfaceTypeDefinition
                             <$> Gen.maybe genDescription
                             <*> genName
                             <*> genDirectives
                             <*> genFieldDefinitions

genUnionTypeDefinition :: Gen UnionTypeDefinition
genUnionTypeDefinition = UnionTypeDefinition
                         <$> Gen.maybe genDescription
                         <*> genName
                         <*> genDirectives
                         <*> Gen.list (Range.linear 1 11) genNamedType

genEnumTypeDefinition :: Gen EnumTypeDefinition
genEnumTypeDefinition = EnumTypeDefinition
                        <$> Gen.maybe genDescription
                        <*> genName
                        <*> genDirectives
                        <*> Gen.list (Range.linear 1 11) genEnumValueDefinition

genInputObjectTypeDefinition :: Gen InputObjectTypeDefinition
genInputObjectTypeDefinition = InputObjectTypeDefinition
                               <$> Gen.maybe genDescription
                               <*> genName
                               <*> genDirectives
                               <*> Gen.list (Range.linear 1 11) genInputValueDefinition

genInputValueDefinition :: Gen InputValueDefinition
genInputValueDefinition = InputValueDefinition
                          <$> Gen.maybe genDescription
                          <*> genName
                          <*> genGType
                          <*> Gen.maybe genDefaultValue

genEnumValueDefinition :: Gen EnumValueDefinition
genEnumValueDefinition = EnumValueDefinition
                         <$> Gen.maybe genDescription
                         <*> genEnumValue
                         <*> genDirectives

genFieldDefinition :: Gen FieldDefinition
genFieldDefinition = FieldDefinition
                     <$> Gen.maybe genDescription
                     <*> genName
                     <*> Gen.list (Range.linear 1 11) genInputValueDefinition
                     <*> genGType
                     <*> genDirectives

genFieldDefinitions :: Gen [FieldDefinition]
genFieldDefinitions = Gen.list (Range.linear 1 11) genFieldDefinition


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
