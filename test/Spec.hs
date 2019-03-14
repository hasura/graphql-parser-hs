{-# LANGUAGE TemplateHaskell #-}

import           Hedgehog
import           Protolude

import qualified Data.Text                      as T
import qualified Data.Text.IO                   as T
import qualified Hedgehog.Gen                   as Gen
import qualified Hedgehog.Range                 as Range

import           Language.GraphQL.Draft.Parser  (parse, parseExecutableDoc,
                                                 value)
import           Language.GraphQL.Draft.Syntax

import qualified Language.GraphQL.Draft.Printer as PP


main :: IO ()
main = do
  res <- tests
  putStrLn $ "Status of tests :: " <> show res

tests :: IO Bool
tests =
  checkParallel $$(discover)

prop_reverse :: Property
prop_reverse =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 1 111) Gen.alpha
    reverse (reverse xs) === xs

prop_parser_printer :: Property
prop_parser_printer =
  property $ do
    xs <- forAll genExecutableDocument
    let rendered = PP.renderPretty $ PP.executableDocument xs
    --either fail (xs ===) $ parseExecutableDoc rendered
    case parseExecutableDoc rendered of
      Left e -> do
        liftIO $ print e
        fail e
      Right v -> xs === v
  where
    fail e = do
      footnote (T.unpack e)
      failure

prop_value :: Property
prop_value =
 withTests 1 $ property $ do
    xs <- forAll genValue
    let rendered = PP.renderPretty $ PP.value xs
    either fail (xs ===) $ (parse value) rendered
  where
    fail e = do
      footnote (T.unpack e)
      failure



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

genDocument :: Gen Document
genDocument =
  Document <$> Gen.list (Range.linear 1 3) genDefinition

genDefinition :: Gen Definition
genDefinition =
  Gen.choice [ DefinitionExecutable <$> genExecutableDefinition
             , DefinitionTypeSystem <$> genTypeSystemDefinition
             ]

genExecutableDocument :: Gen ExecutableDocument
genExecutableDocument =
  ExecutableDocument <$> Gen.list (Range.linear 1 3) genExecutableDefinition

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

genField :: Gen Field
genField = Field
           <$> Gen.maybe genAlias
           <*> genName
           <*> Gen.list (Range.linear 1 11) genArgument
           <*> genDirectives
           <*> genSelectionSet

genAlias :: Gen Alias
genAlias = Alias <$> genName

genFragmentSpread :: Gen FragmentSpread
genFragmentSpread = FragmentSpread
                    <$> genName
                    <*> genDirectives

genInlineFragment :: Gen InlineFragment
genInlineFragment = InlineFragment
                    <$> Gen.maybe genTypeCondition
                    <*> genDirectives
                    <*> genSelectionSet

genFragmentDefinition :: Gen FragmentDefinition
genFragmentDefinition = FragmentDefinition
                        <$> genName
                        <*> genTypeCondition
                        <*> genDirectives
                        <*> genSelectionSet

genTypeCondition :: Gen TypeCondition
genTypeCondition = genNamedType

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
  Gen.choice [ pure OperationTypeQuery
             , pure OperationTypeMutation
             , pure OperationTypeSubscription
             ]

genSchemaDocument :: Gen SchemaDocument
genSchemaDocument =
  SchemaDocument <$> Gen.list (Range.linear 1 5) genTypeDefinition

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

genDirective :: Gen Directive
genDirective = Directive
               <$> genName
               <*> Gen.list (Range.linear 1 11) genArgument

genDirectives :: Gen [Directive]
genDirectives = Gen.list (Range.linear 1 11) genDirective

genArgument :: Gen Argument
genArgument = Argument <$> genName <*> genValue

genValue :: Gen Value
genValue =
  -- TODO: use maxbound of int32/double or something?
  Gen.recursive
  Gen.choice [ pure VNull
             , VInt <$> Gen.int32 (Range.linear 1 99999)
             , VEnum <$> genEnumValue
             , VFloat <$> Gen.double (Range.linearFrac 1.1 999999.99999)
             , VString <$> genStringValue
             , VBoolean <$> Gen.bool
             , VVariable <$> genVariable
             ]
             [ Gen.subtermM (VList <$> genListValue) pure
             , Gen.subtermM (VObject <$> genObjectValue) pure
             ]

genStringValue :: Gen StringValue
genStringValue = StringValue <$> genText

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
  -- TODO: use maxbound of int32/double or something?
  Gen.recursive
  Gen.choice [ pure VCNull
             , VCInt <$> Gen.int32 (Range.linear 1 9)
             , VCEnum <$> genEnumValue
             , VCFloat <$> Gen.double (Range.linearFrac 1.1 9.9)
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

-- genDirectiveDefinition :: Gen.Gen DirectiveDefinition
-- genDirectiveDefinition =
--   DirectiveDefinition <$> Gen.maybe


-- genDirectiveLocation :: Gen DirectiveLocation
-- genDirectiveLocation =
--   Gen.choice [ DLExecutable <$> genExecutableDirectiveLocation
--              , DLTypeSystem <$> genTypeSystemDirectiveLocation
--              ]

-- genExecutableDirectiveLocation :: Gen ExecutableDirectiveLocation
-- genExecutableDirectiveLocation =
--   Gen.choice [ EDLQUERY
--              , EDLMUTATION
--              , EDLSUBSCRIPTION
--              , EDLFIELD
--              , EDLFRAGMENT_DEFINITION
--              , EDLFRAGMENT_SPREAD
--              , EDLINLINE_FRAGMENT
--              ]

-- genTypeSystemDirectiveLocation :: Gen ExecutableDirectiveLocation
-- genTypeSystemDirectiveLocation =
--   Gen.choice [ TSDLSCHEMA
--              , TSDLSCALAR
--              , TSDLOBJECT
--              , TSDLFIELD_DEFINITION
--              , TSDLARGUMENT_DEFINITION
--              , TSDLINTERFACE
--              , TSDLUNION
--              , TSDLENUM
--              , TSDLENUM_VALUE
--              , TSDLINPUT_OBJECT
--              , TSDLINPUT_FIELD_DEFINITION
--              ]

-- helper functions to test in ghci
testFile :: FilePath -> IO ()
testFile fp = do
  contents <- T.readFile fp
  testP contents

testP :: Text -> IO ()
testP str = case parseExecutableDoc str of
  Left err  -> print err
  Right doc -> do
    T.putStrLn $ show doc
    T.putStrLn (T.pack "-------")
    T.putStrLn $ PP.renderPretty $ PP.executableDocument doc
    --T.putStrLn $ renderCompact $ executableDocument doc
