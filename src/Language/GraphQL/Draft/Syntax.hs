{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Description: The GraphQL AST
module Language.GraphQL.Draft.Syntax
  ( -- * Basics
    Name,
    unName,
    mkName,
    unsafeMkName,
    litName,
    Description (..),
    Value (..),
    literal,
    EnumValue (..),
    Directive (..),

    -- * Types
    GType (..),
    getBaseType,
    Nullability (..),
    showGT,
    showLT,
    isNullable,
    isNotNull,
    isListType,

    -- * Documents
    Document (..),
    ExecutableDocument (..),
    SchemaDocument (..),
    SchemaIntrospection (..),

    -- * Definitions
    Definition (..),
    DirectiveDefinition (..),
    DirectiveLocation (..),

    -- ** Type system definitions
    TypeSystemDefinition (..),
    SchemaDefinition (..),
    RootOperationTypeDefinition (..),
    TypeDefinition (..),
    ObjectTypeDefinition (..),
    FieldDefinition (..),
    ArgumentsDefinition,
    InputValueDefinition (..),
    InterfaceTypeDefinition (..),
    UnionTypeDefinition (..),
    ScalarTypeDefinition (..),
    EnumTypeDefinition (..),
    EnumValueDefinition (..),
    InputObjectTypeDefinition (..),
    TypeSystemDirectiveLocation (..),

    -- ** Executable definitions
    ExecutableDefinition (..),
    partitionExDefs,
    OperationDefinition (..),
    OperationType (..),
    TypedOperationDefinition (..),
    VariableDefinition (..),
    ExecutableDirectiveLocation (..),
    FragmentDefinition (..),

    -- * Queries
    SelectionSet,
    Selection (..),
    Field (..),
    FragmentSpread (..),
    NoFragments,
    InlineFragment (..),

    -- ** Fragment conversion functions
    inline,
    fmapFieldFragment,
    fmapSelectionSetFragment,
    fmapSelectionFragment,
    fmapInlineFragment,
  )
where

-------------------------------------------------------------------------------

import Control.DeepSeq (NFData)
import Data.Aeson qualified as J
import Data.Bool (bool)
import Data.Char qualified as C
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.Scientific (Scientific)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import Instances.TH.Lift ()
import {-# SOURCE #-} Language.GraphQL.Draft.Parser
  ( parseExecutableDoc,
    parseSchemaDocument,
  )
import {-# SOURCE #-} Language.GraphQL.Draft.Printer (renderExecutableDoc)
import Language.GraphQL.Draft.Syntax.Internal (liftTypedHashMap)
import Language.Haskell.TH.Syntax (Lift)
import Language.Haskell.TH.Syntax qualified as TH
-- import Language.Haskell.TH.Syntax.Compat (CodeQ, fromCode, liftTypedQuote)
import Language.Haskell.TH.Syntax.Compat
import Prettyprinter (Pretty (..))
import Prelude

-------------------------------------------------------------------------------
newtype Name = Name {unName :: Text}
  deriving stock (Eq, Lift, Ord, Show)
  deriving newtype (Hashable, NFData, Pretty, Semigroup, J.ToJSONKey, J.ToJSON)

mkName :: Text -> Maybe Name
mkName text =
  T.uncons text >>= \(first, body) ->
    if matchFirst first && T.all matchBody body
      then Just (Name text)
      else Nothing
  where
    matchFirst c = c == '_' || C.isAsciiUpper c || C.isAsciiLower c
    matchBody c = c == '_' || C.isAsciiUpper c || C.isAsciiLower c || C.isDigit c

unsafeMkName :: Text -> Name
unsafeMkName = Name

parseName :: MonadFail m => Text -> m Name
parseName text = maybe (fail errorMessage) pure $ mkName text
  where
    errorMessage = T.unpack text <> " is not valid GraphQL name"

litName :: Text -> SpliceQ Name
litName txt = liftSplice do
  name <- parseName txt
  examineSplice [||name||]

-- parseName >=> fromCode . liftTypedQuote

instance J.FromJSON Name where
  parseJSON = J.withText "Name" parseName

instance J.FromJSONKey Name where
  fromJSONKey = J.FromJSONKeyTextParser parseName

-- * Documents

newtype Document = Document {getDefinitions :: [Definition]}
  deriving stock (Eq, Lift, Ord, Show)

data Definition
  = DefinitionExecutable (ExecutableDefinition Name)
  | DefinitionTypeSystem TypeSystemDefinition
  deriving stock (Eq, Generic, Lift, Ord, Show)

instance Hashable Definition

newtype ExecutableDocument var = ExecutableDocument {getExecutableDefinitions :: [ExecutableDefinition var]}
  deriving stock (Eq, Lift, Ord, Show, Functor, Foldable, Traversable)
  deriving newtype (Hashable, NFData)

instance J.FromJSON (ExecutableDocument Name) where
  parseJSON = J.withText "ExecutableDocument" $ \t ->
    case parseExecutableDoc t of
      Right a -> return a
      Left _ -> fail "parsing the graphql query failed"

instance J.ToJSON (ExecutableDocument Name) where
  toJSON = J.String . renderExecutableDoc

data ExecutableDefinition var
  = ExecutableDefinitionOperation (OperationDefinition FragmentSpread var)
  | ExecutableDefinitionFragment FragmentDefinition
  deriving stock (Eq, Generic, Lift, Ord, Show, Functor, Foldable, Traversable)

instance Hashable var => Hashable (ExecutableDefinition var)

instance NFData var => NFData (ExecutableDefinition var)

partitionExDefs ::
  [ExecutableDefinition var] ->
  ( [SelectionSet FragmentSpread var],
    [TypedOperationDefinition FragmentSpread var],
    [FragmentDefinition]
  )
partitionExDefs = foldr f ([], [], [])
  where
    f d (selSets, ops, frags) = case d of
      ExecutableDefinitionOperation (OperationDefinitionUnTyped t) ->
        (t : selSets, ops, frags)
      ExecutableDefinitionOperation (OperationDefinitionTyped t) ->
        (selSets, t : ops, frags)
      ExecutableDefinitionFragment frag ->
        (selSets, ops, frag : frags)

data TypeSystemDefinition
  = TypeSystemDefinitionSchema SchemaDefinition
  | TypeSystemDefinitionType (TypeDefinition () InputValueDefinition) -- No 'possibleTypes' specified for interfaces
  deriving stock (Eq, Generic, Lift, Ord, Show)
  deriving anyclass (Hashable, NFData)

data SchemaDefinition = SchemaDefinition
  { _sdDirectives :: Maybe [Directive Void],
    _sdRootOperationTypeDefinitions :: [RootOperationTypeDefinition]
  }
  deriving stock (Eq, Generic, Lift, Ord, Show)
  deriving anyclass (Hashable, NFData)

data RootOperationTypeDefinition = RootOperationTypeDefinition
  { _rotdOperationType :: OperationType,
    _rotdOperationTypeType :: Name
  }
  deriving stock (Eq, Generic, Lift, Ord, Show)
  deriving anyclass (Hashable, NFData)

data OperationType
  = OperationTypeQuery
  | OperationTypeMutation
  | OperationTypeSubscription
  deriving stock (Eq, Generic, Lift, Ord, Show)
  deriving anyclass (Hashable, NFData)

newtype SchemaDocument
  = SchemaDocument [TypeSystemDefinition]
  deriving stock (Eq, Generic, Lift, Ord, Show)
  deriving newtype (Hashable, NFData)

instance J.FromJSON SchemaDocument where
  parseJSON = J.withText "SchemaDocument" $ \t ->
    case parseSchemaDocument t of
      Right schemaDoc -> return schemaDoc
      Left err -> fail $ "parsing the schema document: " <> show err

-- | A variant of 'SchemaDocument' that additionally stores, for each interface,
-- the list of object types that implement that interface. Types are indexed by
-- their name for fast lookups.
newtype SchemaIntrospection
  = SchemaIntrospection (HashMap Name (TypeDefinition [Name] InputValueDefinition))
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (Hashable)

data OperationDefinition frag var
  = OperationDefinitionTyped (TypedOperationDefinition frag var)
  | OperationDefinitionUnTyped (SelectionSet frag var)
  deriving stock (Eq, Generic, Lift, Ord, Show, Functor, Foldable, Traversable)
  deriving anyclass (Hashable, NFData)

data TypedOperationDefinition frag var = TypedOperationDefinition
  { _todType :: OperationType,
    _todName :: Maybe Name,
    _todVariableDefinitions :: [VariableDefinition],
    _todDirectives :: [Directive var],
    _todSelectionSet :: SelectionSet frag var
  }
  deriving stock (Eq, Generic, Lift, Ord, Show, Functor, Foldable, Traversable)
  deriving anyclass (Hashable, NFData)

data VariableDefinition = VariableDefinition
  { _vdName :: Name,
    _vdType :: GType,
    _vdDefaultValue :: Maybe (Value Void)
  }
  deriving stock (Eq, Generic, Lift, Ord, Show)
  deriving anyclass (Hashable, NFData)

type SelectionSet frag var = [Selection frag var]

data Selection frag var
  = SelectionField (Field frag var)
  | SelectionFragmentSpread (frag var)
  | SelectionInlineFragment (InlineFragment frag var)
  deriving stock (Eq, Generic, Lift, Ord, Show, Functor, Foldable, Traversable)
  deriving anyclass (Hashable, NFData)

data Field frag var = Field
  { _fAlias :: Maybe Name,
    _fName :: Name,
    _fArguments :: HashMap Name (Value var),
    _fDirectives :: [Directive var],
    _fSelectionSet :: SelectionSet frag var
  }
  deriving stock (Eq, Generic, Ord, Show, Functor, Foldable, Traversable)
  deriving anyclass (Hashable, NFData)

instance (Lift (frag var), Lift var) => Lift (Field frag var) where
  liftTyped Field {..} =
    [||
    Field
      { _fAlias,
        _fName,
        _fDirectives,
        _fSelectionSet,
        _fArguments = $$(liftTypedHashMap _fArguments)
      }
    ||]

-- * Fragments

data FragmentSpread var = FragmentSpread
  { _fsName :: Name,
    _fsDirectives :: [Directive var]
  }
  deriving stock (Eq, Generic, Lift, Ord, Show, Functor, Foldable, Traversable)
  deriving anyclass (Hashable, NFData)

-- | Can be used in place of the @frag@ parameter to various AST types to
-- guarante that the AST does not include any fragment spreads.
--
-- Note: This is equivalent to @'Const' 'Void'@, but annoyingly, 'Const' does
-- not provide a 'Lift' instance as of GHC 8.6.
data NoFragments var
  deriving stock (Eq, Generic, Lift, Ord, Show, Functor, Foldable, Traversable)
  deriving anyclass (Hashable, NFData)

data InlineFragment frag var = InlineFragment
  { _ifTypeCondition :: Maybe Name,
    _ifDirectives :: [Directive var],
    _ifSelectionSet :: SelectionSet frag var
  }
  deriving stock (Eq, Generic, Lift, Ord, Show, Functor, Foldable, Traversable)
  deriving anyclass (Hashable, NFData)

data FragmentDefinition = FragmentDefinition
  { _fdName :: Name,
    _fdTypeCondition :: Name,
    _fdDirectives :: [Directive Name],
    _fdSelectionSet :: SelectionSet FragmentSpread Name
  }
  deriving stock (Eq, Generic, Lift, Ord, Show)
  deriving anyclass (Hashable, NFData)

-- * Values

data Value var
  = VVariable var
  | VNull
  | VInt Integer
  | VFloat Scientific
  | VString Text
  | VBoolean Bool
  | VEnum EnumValue
  | VList [Value var]
  | VObject (HashMap Name (Value var))
  deriving stock (Eq, Generic, Ord, Show, Functor, Foldable, Traversable)
  deriving anyclass (Hashable, NFData)

instance Lift var => Lift (Value var) where
  liftTyped (VVariable a) = [||VVariable a||]
  liftTyped VNull = [||VNull||]
  liftTyped (VInt a) = [||VInt a||]
  liftTyped (VFloat a) = [||VFloat $ fromRational $$(TH.liftTyped $ toRational a)||]
  liftTyped (VString a) = [||VString a||]
  liftTyped (VBoolean a) = [||VBoolean a||]
  liftTyped (VEnum a) = [||VEnum a||]
  liftTyped (VList a) = [||VList a||]
  liftTyped (VObject a) = [||VObject $$(liftTypedHashMap a)||]

literal :: Value Void -> Value var
literal = fmap absurd

-- * Directives

data Directive var = Directive
  { _dName :: Name,
    _dArguments :: HashMap Name (Value var)
  }
  deriving stock (Eq, Generic, Ord, Show, Functor, Foldable, Traversable)
  deriving anyclass (Hashable, NFData)

instance Lift var => Lift (Directive var) where
  liftTyped Directive {..} =
    [||
    Directive
      { _dName,
        _dArguments = $$(liftTypedHashMap _dArguments)
      }
    ||]

--     * Type Reference

newtype Nullability = Nullability {unNullability :: Bool}
  deriving stock (Eq, Generic, Lift, Ord, Show)
  deriving newtype (Hashable, NFData)

data GType
  = TypeNamed Nullability Name
  | TypeList Nullability GType
  deriving stock (Eq, Generic, Lift, Ord, Show)
  deriving anyclass (Hashable, NFData)

getBaseType :: GType -> Name
getBaseType = \case
  TypeNamed _ namedType -> namedType
  TypeList _ listType -> getBaseType listType

instance J.ToJSON GType where
  toJSON = J.toJSON . showGT

showGT :: GType -> Text
showGT = \case
  TypeNamed nullability nt -> unName nt <> showNullable nullability
  TypeList nullability lt -> showLT lt <> showNullable nullability

showNullable :: Nullability -> Text
showNullable = bool "!" "" . unNullability

showLT :: GType -> Text
showLT lt = "[" <> showGT lt <> "]"

isNullable :: GType -> Bool
isNullable = \case
  TypeNamed nullability _ -> unNullability nullability
  TypeList nullability _ -> unNullability nullability

isListType :: GType -> Bool
isListType = \case
  TypeList _ _ -> True
  TypeNamed _ _ -> False

isNotNull :: GType -> Bool
isNotNull = not . isNullable

-- * Type definition

data TypeDefinition possibleTypes inputType
  = TypeDefinitionScalar ScalarTypeDefinition
  | TypeDefinitionObject (ObjectTypeDefinition inputType)
  | TypeDefinitionInterface (InterfaceTypeDefinition possibleTypes inputType)
  | TypeDefinitionUnion UnionTypeDefinition
  | TypeDefinitionEnum EnumTypeDefinition
  | TypeDefinitionInputObject (InputObjectTypeDefinition inputType)
  deriving stock (Eq, Generic, Lift, Ord, Show, Functor)
  deriving anyclass (Hashable, NFData)

newtype Description = Description {unDescription :: Text}
  deriving stock (Eq, Lift, Ord, Show)
  deriving newtype (Hashable, IsString, Monoid, NFData, Semigroup, J.FromJSON, J.ToJSON)

data ObjectTypeDefinition inputType = ObjectTypeDefinition
  { _otdDescription :: Maybe Description,
    _otdName :: Name,
    _otdImplementsInterfaces :: [Name],
    _otdDirectives :: [Directive Void],
    _otdFieldsDefinition :: [FieldDefinition inputType]
  }
  deriving stock (Eq, Generic, Lift, Ord, Show, Functor)
  deriving anyclass (Hashable, NFData)

data FieldDefinition inputType = FieldDefinition
  { _fldDescription :: Maybe Description,
    _fldName :: Name,
    _fldArgumentsDefinition :: (ArgumentsDefinition inputType),
    _fldType :: GType,
    _fldDirectives :: [Directive Void]
  }
  deriving stock (Eq, Generic, Lift, Ord, Show, Functor)
  deriving anyclass (Hashable, NFData)

type ArgumentsDefinition inputType = [inputType]

data InputValueDefinition = InputValueDefinition
  { _ivdDescription :: Maybe Description,
    _ivdName :: Name,
    _ivdType :: GType,
    _ivdDefaultValue :: Maybe (Value Void),
    _ivdDirectives :: [Directive Void]
  }
  deriving stock (Eq, Generic, Lift, Ord, Show)
  deriving anyclass (Hashable, NFData)

data InterfaceTypeDefinition possibleTypes inputType = InterfaceTypeDefinition
  { _itdDescription :: Maybe Description,
    _itdName :: Name,
    _itdDirectives :: [Directive Void],
    _itdFieldsDefinition :: [FieldDefinition inputType],
    _itdPossibleTypes :: possibleTypes
  }
  deriving stock (Eq, Generic, Lift, Ord, Show, Functor)
  deriving anyclass (Hashable, NFData)

data UnionTypeDefinition = UnionTypeDefinition
  { _utdDescription :: Maybe Description,
    _utdName :: Name,
    _utdDirectives :: [Directive Void],
    _utdMemberTypes :: [Name]
  }
  deriving stock (Eq, Generic, Lift, Ord, Show)
  deriving anyclass (Hashable, NFData)

data ScalarTypeDefinition = ScalarTypeDefinition
  { _stdDescription :: Maybe Description,
    _stdName :: Name,
    _stdDirectives :: [Directive Void]
  }
  deriving stock (Eq, Generic, Lift, Ord, Show)
  deriving anyclass (Hashable, NFData)

data EnumTypeDefinition = EnumTypeDefinition
  { _etdDescription :: Maybe Description,
    _etdName :: Name,
    _etdDirectives :: [Directive Void],
    _etdValueDefinitions :: [EnumValueDefinition]
  }
  deriving stock (Eq, Generic, Lift, Ord, Show)
  deriving anyclass (Hashable, NFData)

data EnumValueDefinition = EnumValueDefinition
  { _evdDescription :: Maybe Description,
    _evdName :: EnumValue,
    _evdDirectives :: [Directive Void]
  }
  deriving stock (Eq, Generic, Lift, Ord, Show)
  deriving anyclass (Hashable, NFData)

newtype EnumValue = EnumValue {unEnumValue :: Name}
  deriving stock (Eq, Lift, Ord, Show)
  deriving newtype (Hashable, NFData, J.ToJSON, J.FromJSON)

data InputObjectTypeDefinition inputType = InputObjectTypeDefinition
  { _iotdDescription :: Maybe Description,
    _iotdName :: Name,
    _iotdDirectives :: [Directive Void],
    _iotdValueDefinitions :: [inputType]
  }
  deriving stock (Eq, Generic, Lift, Ord, Show, Functor)
  deriving anyclass (Hashable, NFData)

data DirectiveDefinition inputType = DirectiveDefinition
  { _ddDescription :: Maybe Description,
    _ddName :: Name,
    _ddArguments :: (ArgumentsDefinition inputType),
    _ddLocations :: [DirectiveLocation]
  }
  deriving stock (Eq, Generic, Lift, Ord, Show, Functor)
  deriving anyclass (Hashable, NFData)

data DirectiveLocation
  = DLExecutable ExecutableDirectiveLocation
  | DLTypeSystem TypeSystemDirectiveLocation
  deriving stock (Eq, Generic, Lift, Ord, Show)
  deriving anyclass (Hashable, NFData)

data ExecutableDirectiveLocation
  = EDLQUERY
  | EDLMUTATION
  | EDLSUBSCRIPTION
  | EDLFIELD
  | EDLFRAGMENT_DEFINITION
  | EDLFRAGMENT_SPREAD
  | EDLINLINE_FRAGMENT
  deriving stock (Eq, Generic, Lift, Ord, Show)
  deriving anyclass (Hashable, NFData)

data TypeSystemDirectiveLocation
  = TSDLSCHEMA
  | TSDLSCALAR
  | TSDLOBJECT
  | TSDLFIELD_DEFINITION
  | TSDLARGUMENT_DEFINITION
  | TSDLINTERFACE
  | TSDLUNION
  | TSDLENUM
  | TSDLENUM_VALUE
  | TSDLINPUT_OBJECT
  | TSDLINPUT_FIELD_DEFINITION
  deriving stock (Eq, Generic, Lift, Ord, Show)
  deriving anyclass (Hashable, NFData)

inline :: NoFragments var -> FragmentSpread var
inline x = case x of {}

fmapFieldFragment :: (frag var -> frag' var) -> Field frag var -> Field frag' var
fmapFieldFragment f field =
  field {_fSelectionSet = fmapSelectionSetFragment f (_fSelectionSet field)}

fmapSelectionSetFragment :: (frag var -> frag' var) -> SelectionSet frag var -> SelectionSet frag' var
fmapSelectionSetFragment f = fmap (fmapSelectionFragment f)

fmapSelectionFragment :: (frag var -> frag' var) -> Selection frag var -> Selection frag' var
fmapSelectionFragment f (SelectionField field) = SelectionField $ fmapFieldFragment f field
fmapSelectionFragment f (SelectionFragmentSpread frag) = SelectionFragmentSpread $ f frag
fmapSelectionFragment f (SelectionInlineFragment inlineFrag) =
  SelectionInlineFragment $ fmapInlineFragment f inlineFrag

fmapInlineFragment :: (frag var -> frag' var) -> InlineFragment frag var -> InlineFragment frag' var
fmapInlineFragment f inlineFragment =
  inlineFragment {_ifSelectionSet = fmapSelectionSetFragment f (_ifSelectionSet inlineFragment)}
