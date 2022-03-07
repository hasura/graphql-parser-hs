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
import Control.Monad ((>=>))
import Data.Aeson qualified as J
import Data.Bool (bool)
import Data.Char qualified as C
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as M
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
import Language.Haskell.TH.Syntax (Lift, Q)
import Language.Haskell.TH.Syntax qualified as TH
import Prettyprinter (Pretty (..))

-------------------------------------------------------------------------------

newtype Name = Name {unName :: Text}
  deriving (Eq, Ord, Show, Hashable, NFData, Lift, Semigroup, J.ToJSONKey, J.ToJSON)

instance Pretty Name where
  pretty = pretty . unName

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

litName :: Text -> Q (TH.TExp Name)
litName = parseName >=> \name -> [||name||]

instance J.FromJSON Name where
  parseJSON = J.withText "Name" parseName

instance J.FromJSONKey Name where
  fromJSONKey = J.FromJSONKeyTextParser parseName

-- * Documents

newtype Document = Document {getDefinitions :: [Definition]}
  deriving (Ord, Show, Eq, Lift)

data Definition
  = DefinitionExecutable (ExecutableDefinition Name)
  | DefinitionTypeSystem TypeSystemDefinition
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable Definition

newtype ExecutableDocument var = ExecutableDocument {getExecutableDefinitions :: [ExecutableDefinition var]}
  deriving (Ord, Show, Eq, Lift, Hashable, NFData, Functor, Foldable, Traversable)

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
  deriving (Ord, Show, Eq, Lift, Functor, Foldable, Traversable, Generic)

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
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable TypeSystemDefinition

instance NFData TypeSystemDefinition

data SchemaDefinition = SchemaDefinition
  { _sdDirectives :: Maybe [Directive Void],
    _sdRootOperationTypeDefinitions :: [RootOperationTypeDefinition]
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable SchemaDefinition

instance NFData SchemaDefinition

data RootOperationTypeDefinition = RootOperationTypeDefinition
  { _rotdOperationType :: OperationType,
    _rotdOperationTypeType :: Name
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable RootOperationTypeDefinition

instance NFData RootOperationTypeDefinition

data OperationType
  = OperationTypeQuery
  | OperationTypeMutation
  | OperationTypeSubscription
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable OperationType

instance NFData OperationType

newtype SchemaDocument
  = SchemaDocument [TypeSystemDefinition]
  deriving (Ord, Show, Eq, Lift, Hashable, NFData, Generic)

instance J.FromJSON SchemaDocument where
  parseJSON = J.withText "SchemaDocument" $ \t ->
    case parseSchemaDocument t of
      Right schemaDoc -> return schemaDoc
      Left err -> fail $ "parsing the schema document: " <> show err

-- | A variant of 'SchemaDocument' that additionally stores, for each interface,
-- the list of object types that implement that interface
newtype SchemaIntrospection
  = SchemaIntrospection [TypeDefinition [Name] InputValueDefinition]
  deriving (Ord, Show, Eq, Lift, Hashable, Generic)

data OperationDefinition frag var
  = OperationDefinitionTyped (TypedOperationDefinition frag var)
  | OperationDefinitionUnTyped (SelectionSet frag var)
  deriving (Ord, Show, Eq, Lift, Functor, Foldable, Traversable, Generic)

instance (Hashable (frag var), Hashable var) => Hashable (OperationDefinition frag var)

instance (NFData (frag var), NFData var) => NFData (OperationDefinition frag var)

data TypedOperationDefinition frag var = TypedOperationDefinition
  { _todType :: OperationType,
    _todName :: Maybe Name,
    _todVariableDefinitions :: [VariableDefinition],
    _todDirectives :: [Directive var],
    _todSelectionSet :: SelectionSet frag var
  }
  deriving (Ord, Show, Eq, Lift, Functor, Foldable, Traversable, Generic)

instance (Hashable (frag var), Hashable var) => Hashable (TypedOperationDefinition frag var)

instance (NFData (frag var), NFData var) => NFData (TypedOperationDefinition frag var)

data VariableDefinition = VariableDefinition
  { _vdName :: Name,
    _vdType :: GType,
    _vdDefaultValue :: Maybe (Value Void)
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable VariableDefinition

instance NFData VariableDefinition

type SelectionSet frag var = [Selection frag var]

data Selection frag var
  = SelectionField (Field frag var)
  | SelectionFragmentSpread (frag var)
  | SelectionInlineFragment (InlineFragment frag var)
  deriving (Ord, Show, Eq, Lift, Functor, Foldable, Traversable, Generic)

instance (Hashable (frag var), Hashable var) => Hashable (Selection frag var)

instance (NFData (frag var), NFData var) => NFData (Selection frag var)

data Field frag var = Field
  { _fAlias :: Maybe Name,
    _fName :: Name,
    _fArguments :: HashMap Name (Value var),
    _fDirectives :: [Directive var],
    _fSelectionSet :: SelectionSet frag var
  }
  deriving (Ord, Show, Eq, Functor, Foldable, Traversable, Generic)

instance (Hashable (frag var), Hashable var) => Hashable (Field frag var)

instance (NFData (frag var), NFData var) => NFData (Field frag var)

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
  deriving (Ord, Show, Eq, Lift, Functor, Foldable, Traversable, Generic)

instance Hashable var => Hashable (FragmentSpread var)

instance NFData var => NFData (FragmentSpread var)

-- | Can be used in place of the @frag@ parameter to various AST types to
-- guarante that the AST does not include any fragment spreads.
--
-- Note: This is equivalent to @'Const' 'Void'@, but annoyingly, 'Const' does
-- not provide a 'Lift' instance as of GHC 8.6.
data NoFragments var
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Lift, Generic)

instance Hashable (NoFragments var)

instance NFData (NoFragments var)

data InlineFragment frag var = InlineFragment
  { _ifTypeCondition :: Maybe Name,
    _ifDirectives :: [Directive var],
    _ifSelectionSet :: SelectionSet frag var
  }
  deriving (Ord, Show, Eq, Lift, Functor, Foldable, Traversable, Generic)

instance (Hashable (frag var), Hashable var) => Hashable (InlineFragment frag var)

instance (NFData (frag var), NFData var) => NFData (InlineFragment frag var)

data FragmentDefinition = FragmentDefinition
  { _fdName :: Name,
    _fdTypeCondition :: Name,
    _fdDirectives :: [Directive Name],
    _fdSelectionSet :: SelectionSet FragmentSpread Name
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable FragmentDefinition

instance NFData FragmentDefinition

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
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

instance Hashable var => Hashable (Value var)

instance NFData var => NFData (Value var)

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
  deriving (Ord, Show, Eq, Functor, Foldable, Traversable, Generic)

instance Hashable var => Hashable (Directive var)

instance NFData var => NFData (Directive var)

instance Lift var => Lift (Directive var) where
  liftTyped Directive {..} = [||Directive {_dName, _dArguments = $$(liftTypedHashMap _dArguments)}||]

-- * Type Reference

newtype Nullability = Nullability {unNullability :: Bool}
  deriving (Show, Ord, Eq, Lift, Generic, Hashable, NFData)

data GType
  = TypeNamed Nullability Name
  | TypeList Nullability GType
  deriving (Eq, Ord, Show, Lift, Generic)

getBaseType :: GType -> Name
getBaseType = \case
  TypeNamed _ namedType -> namedType
  TypeList _ listType -> getBaseType listType

instance J.ToJSON GType where
  toJSON = J.toJSON . showGT

instance Hashable GType

instance NFData GType

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
  deriving (Ord, Show, Eq, Lift, Generic, Functor)

instance (Hashable possibleTypes, Hashable inputType) => Hashable (TypeDefinition possibleTypes inputType)

instance (NFData possibleTypes, NFData inputType) => NFData (TypeDefinition possibleTypes inputType)

newtype Description = Description {unDescription :: Text}
  deriving (Show, Eq, Ord, IsString, Lift, Semigroup, Monoid, Hashable, NFData, J.ToJSON, J.FromJSON)

data ObjectTypeDefinition inputType = ObjectTypeDefinition
  { _otdDescription :: Maybe Description,
    _otdName :: Name,
    _otdImplementsInterfaces :: [Name],
    _otdDirectives :: [Directive Void],
    _otdFieldsDefinition :: [FieldDefinition inputType]
  }
  deriving (Ord, Show, Eq, Lift, Generic, Functor)

instance (Hashable inputType) => Hashable (ObjectTypeDefinition inputType)

instance (NFData inputType) => NFData (ObjectTypeDefinition inputType)

data FieldDefinition inputType = FieldDefinition
  { _fldDescription :: Maybe Description,
    _fldName :: Name,
    _fldArgumentsDefinition :: (ArgumentsDefinition inputType),
    _fldType :: GType,
    _fldDirectives :: [Directive Void]
  }
  deriving (Ord, Show, Eq, Lift, Generic, Functor)

instance (Hashable inputType) => Hashable (FieldDefinition inputType)

instance (NFData inputType) => NFData (FieldDefinition inputType)

type ArgumentsDefinition inputType = [inputType]

data InputValueDefinition = InputValueDefinition
  { _ivdDescription :: Maybe Description,
    _ivdName :: Name,
    _ivdType :: GType,
    _ivdDefaultValue :: Maybe (Value Void),
    _ivdDirectives :: [Directive Void]
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable InputValueDefinition

instance NFData InputValueDefinition

data InterfaceTypeDefinition possibleTypes inputType = InterfaceTypeDefinition
  { _itdDescription :: Maybe Description,
    _itdName :: Name,
    _itdDirectives :: [Directive Void],
    _itdFieldsDefinition :: [FieldDefinition inputType],
    _itdPossibleTypes :: possibleTypes
  }
  deriving (Ord, Show, Eq, Lift, Generic, Functor)

instance (Hashable possibleTypes, Hashable inputType) => Hashable (InterfaceTypeDefinition possibleTypes inputType)

instance (NFData possibleTypes, NFData inputType) => NFData (InterfaceTypeDefinition possibleTypes inputType)

data UnionTypeDefinition = UnionTypeDefinition
  { _utdDescription :: Maybe Description,
    _utdName :: Name,
    _utdDirectives :: [Directive Void],
    _utdMemberTypes :: [Name]
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable UnionTypeDefinition

instance NFData UnionTypeDefinition

data ScalarTypeDefinition = ScalarTypeDefinition
  { _stdDescription :: Maybe Description,
    _stdName :: Name,
    _stdDirectives :: [Directive Void]
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable ScalarTypeDefinition

instance NFData ScalarTypeDefinition

data EnumTypeDefinition = EnumTypeDefinition
  { _etdDescription :: Maybe Description,
    _etdName :: Name,
    _etdDirectives :: [Directive Void],
    _etdValueDefinitions :: [EnumValueDefinition]
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable EnumTypeDefinition

instance NFData EnumTypeDefinition

data EnumValueDefinition = EnumValueDefinition
  { _evdDescription :: Maybe Description,
    _evdName :: EnumValue,
    _evdDirectives :: [Directive Void]
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable EnumValueDefinition

instance NFData EnumValueDefinition

newtype EnumValue = EnumValue {unEnumValue :: Name}
  deriving (Show, Eq, Lift, Hashable, NFData, J.ToJSON, J.FromJSON, Ord)

data InputObjectTypeDefinition inputType = InputObjectTypeDefinition
  { _iotdDescription :: Maybe Description,
    _iotdName :: Name,
    _iotdDirectives :: [Directive Void],
    _iotdValueDefinitions :: [inputType]
  }
  deriving (Ord, Show, Eq, Lift, Generic, Functor)

instance (Hashable inputType) => Hashable (InputObjectTypeDefinition inputType)

instance (NFData inputType) => NFData (InputObjectTypeDefinition inputType)

data DirectiveDefinition inputType = DirectiveDefinition
  { _ddDescription :: Maybe Description,
    _ddName :: Name,
    _ddArguments :: (ArgumentsDefinition inputType),
    _ddLocations :: [DirectiveLocation]
  }
  deriving (Ord, Show, Eq, Lift, Generic)

instance (Hashable inputType) => Hashable (DirectiveDefinition inputType)

instance (NFData inputType) => NFData (DirectiveDefinition inputType)

data DirectiveLocation
  = DLExecutable ExecutableDirectiveLocation
  | DLTypeSystem TypeSystemDirectiveLocation
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable DirectiveLocation

instance NFData DirectiveLocation

data ExecutableDirectiveLocation
  = EDLQUERY
  | EDLMUTATION
  | EDLSUBSCRIPTION
  | EDLFIELD
  | EDLFRAGMENT_DEFINITION
  | EDLFRAGMENT_SPREAD
  | EDLINLINE_FRAGMENT
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable ExecutableDirectiveLocation

instance NFData ExecutableDirectiveLocation

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
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable TypeSystemDirectiveLocation

instance NFData TypeSystemDirectiveLocation

liftTypedHashMap :: (Hashable k, Lift k, Lift v) => HashMap k v -> Q (TH.TExp (HashMap k v))
liftTypedHashMap a = [||M.fromList $$(TH.liftTyped $ M.toList a)||]

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
