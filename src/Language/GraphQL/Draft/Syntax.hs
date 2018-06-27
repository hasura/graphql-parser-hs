{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | Description: The GraphQL AST
module Language.GraphQL.Draft.Syntax
  ( Name(..)
  , Document(..)
  , ExecutableDocument(..)
  , SchemaDocument(..)
  , Definition(..)
  , ExecutableDefinition(..)
  , partitionExDefs
  , OperationDefinition(..)
  , OperationType(..)
  , TypedOperationDefinition(..)
  , VariableDefinition(..)
  , Variable(..)
  , SelectionSet
  , Selection(..)
  , Field(..)
  , Alias(..)
  , Argument(..)
  , FragmentSpread(..)
  , InlineFragment(..)
  , FragmentDefinition(..)
  , TypeCondition
  , ValueConst(..)
  , Value(..)
  , StringValue(..)
  , ListValueG(..)
  , ListValue
  , ListValueC
  , ObjectValueG(..)
  , ObjectValue
  , ObjectValueC
  , ObjectFieldG(..)
  , ObjectField
  , ObjectFieldC
  , DefaultValue
  , Directive(..)
  , GType(..)
  , showGT
  , ToGType(..)
  , toLT
  , showLT
  , ToNonNullType(..)
  , isNotNull
  , showNT
  , NamedType(..)
  , ListType(..)
  , NonNullType(..)
  , showNNT
  , Description(..)
  , TypeDefinition(..)
  , ObjectTypeDefinition(..)
  , FieldDefinition(..)
  , ArgumentsDefinition
  , InputValueDefinition(..)
  , InterfaceTypeDefinition(..)
  , UnionTypeDefinition(..)
  , ScalarTypeDefinition(..)
  , EnumTypeDefinition(..)
  , EnumValueDefinition(..)
  , EnumValue(..)
  , InputObjectTypeDefinition(..)
  , DirectiveDefinition(..)
  , DirectiveLocation(..)
  , ExecutableDirectiveLocation(..)
  , TypeSystemDirectiveLocation(..)
  ) where

import           Instances.TH.Lift          ()
import           Language.Haskell.TH.Syntax (Lift)
import           Protolude

import qualified Data.Aeson                 as J

-- * Documents

-- | A 'QueryDocument' is something a user might send us.
--
-- https://facebook.github.io/graphql/#sec-Language.Query-Document

newtype Name
  = Name { unName :: Text }
  deriving ( Eq, Ord, Show, Hashable, IsString, Lift, Semigroup
           , Monoid, J.ToJSONKey, J.FromJSONKey, J.ToJSON, J.FromJSON)

newtype Document
  = Document { getDefinitions :: [Definition] }
  deriving (Show, Eq, Lift)

data Definition
  = DefinitionExecutable !ExecutableDefinition
  | DefinitionTypeSystem !TypeSystemDefinition
  deriving (Show, Eq, Lift)

newtype ExecutableDocument
  = ExecutableDocument { getExecutableDefinitions :: [ExecutableDefinition] }
  deriving (Show, Eq, Lift)

data ExecutableDefinition
  = ExecutableDefinitionOperation OperationDefinition
  | ExecutableDefinitionFragment FragmentDefinition
  deriving (Show, Eq, Lift)

partitionExDefs
  :: [ExecutableDefinition]
  -> ([SelectionSet], [TypedOperationDefinition], [FragmentDefinition])
partitionExDefs =
  foldr f ([], [], [])
  where
    f d (selSets, ops, frags) = case d of
      ExecutableDefinitionOperation (OperationDefinitionUnTyped t) ->
        (t:selSets, ops, frags)
      ExecutableDefinitionOperation (OperationDefinitionTyped t) ->
        (selSets, t:ops, frags)
      ExecutableDefinitionFragment frag ->
        (selSets, ops, frag:frags)

data TypeSystemDefinition
  = TypeSystemDefinitionSchema !SchemaDefinition
  | TypeSystemDefinitionType !TypeDefinition
  -- | TypeSystemDefinitionDir !DirectiveDefinition
  deriving (Show, Eq, Lift)


data SchemaDefinition
  = SchemaDefinition
  { _sdDirectives                   :: !(Maybe [Directive])
  , _sdRootOperationTypeDefinitions :: ![RootOperationTypeDefinition]
  } deriving (Show, Eq, Lift)

data RootOperationTypeDefinition
  = RootOperationTypeDefinition
  { _rotdOperationType     :: !OperationType
  , _rotdOperationTypeType :: !NamedType
  } deriving (Show, Eq, Lift)

data OperationType
  = OperationTypeQuery
  | OperationTypeMutation
  | OperationTypeSubscription
  deriving (Show, Eq, Lift)

newtype SchemaDocument
  = SchemaDocument [TypeDefinition]
  deriving (Show, Eq, Lift)

data OperationDefinition
  = OperationDefinitionTyped !TypedOperationDefinition
  | OperationDefinitionUnTyped !SelectionSet
  deriving (Show, Eq, Lift)

data TypedOperationDefinition
  = TypedOperationDefinition
  { _todType                :: !OperationType
  , _todName                :: !(Maybe Name)
  , _todVariableDefinitions :: ![VariableDefinition]
  , _todDirectives          :: ![Directive]
  , _todSelectionSet        :: !SelectionSet
  } deriving (Show, Eq, Lift)

data VariableDefinition
  = VariableDefinition
  { _vdVariable     :: !Variable
  , _vdType         :: !GType
  , _vdDefaultValue :: !(Maybe DefaultValue)
  } deriving (Show, Eq, Lift)

newtype Variable
  = Variable { unVariable :: Name }
  deriving (Eq, Ord, Show, Hashable, Lift, J.ToJSONKey, J.FromJSONKey)

type SelectionSet = [Selection]

data Selection
  = SelectionField !Field
  | SelectionFragmentSpread !FragmentSpread
  | SelectionInlineFragment !InlineFragment
  deriving (Show, Eq, Lift)

data Field
  = Field
  { _fAlias        :: !(Maybe Alias)
  , _fName         :: !Name
  , _fArguments    :: ![Argument]
  , _fDirectives   :: ![Directive]
  , _fSelectionSet :: !SelectionSet
  } deriving (Show, Eq, Lift)

newtype Alias
  = Alias { unAlias :: Name }
  deriving (Show, Eq, Hashable, Lift, J.ToJSON, J.FromJSON)

data Argument
  = Argument
  { _aName  :: !Name
  , _aValue :: !Value
  } deriving (Show, Eq, Lift)

-- * Fragments

data FragmentSpread
  = FragmentSpread
  { _fsName       :: !Name
  , _fsDirectives :: ![Directive]
  } deriving (Show, Eq, Lift)

data InlineFragment
  = InlineFragment
  { _ifTypeCondition :: !(Maybe TypeCondition)
  , _ifDirectives    :: ![Directive]
  , _ifSelectionSet  :: !SelectionSet
  } deriving (Show, Eq, Lift)

data FragmentDefinition
  = FragmentDefinition
  { _fdName          :: !Name
  , _fdTypeCondition :: !TypeCondition
  , _fdDirectives    :: ![Directive]
  , _fdSelectionSet  :: !SelectionSet
  } deriving (Show, Eq, Lift)

type TypeCondition = NamedType

-- * Values

-- data ValueLeaf
--   = VLInt !Int32
--   | VLFloat !Double
--   | VLBoolean !Bool
--   | VLString !StringValue
--   | VLEnum !EnumValue
--   | VLNull
--   deriving (Show, Eq, Lift)

-- data ValueConst
--   = VCLeaf !ValueLeaf
--   | VCList !ListValueC
--   | VCObject !ObjectValueC
--   deriving (Show, Eq, Lift)

-- data Value
--   = VVariable !Variable
--   | VLeaf !ValueLeaf
--   | VList !ListValue
--   | VObject !ObjectValue
--   deriving (Show, Eq, Lift)

data ValueConst
  = VCInt !Int32
  | VCFloat !Double
  | VCString !StringValue
  | VCBoolean !Bool
  | VCNull
  | VCEnum !EnumValue
  | VCList !ListValueC
  | VCObject !ObjectValueC
  deriving (Show, Eq, Lift)

data Value
  = VVariable !Variable
  | VInt !Int32
  | VFloat !Double
  | VString !StringValue
  | VBoolean !Bool
  | VNull
  | VEnum !EnumValue
  | VList !ListValue
  | VObject !ObjectValue
  deriving (Show, Eq, Lift)

newtype StringValue
  = StringValue { unStringValue :: Text } deriving (Show, Eq, Lift)

newtype ListValueG a
  = ListValueG {unListValue :: [a]}
  deriving (Show, Eq, Lift)

type ListValue = ListValueG Value

type ListValueC = ListValueG ValueConst

newtype ObjectValueG a
  = ObjectValueG {unObjectValue :: [ObjectFieldG a]} deriving (Show, Eq, Lift)

type ObjectValue = ObjectValueG Value

type ObjectValueC = ObjectValueG ValueConst

data ObjectFieldG a
  = ObjectFieldG
  { _ofName  :: Name
  , _ofValue :: a
  } deriving (Show, Eq, Lift, Functor, Foldable, Traversable)

type ObjectField = ObjectFieldG Value
type ObjectFieldC = ObjectFieldG ValueConst

type DefaultValue = ValueConst

-- * Directives

data Directive = Directive Name [Argument] deriving (Show, Eq, Lift)

-- * Type Reference

data GType
  = TypeNamed NamedType
  | TypeList ListType
  | TypeNonNull NonNullType
  deriving (Eq, Ord, Show, Lift)

showGT :: GType -> Text
showGT = \case
  TypeNamed nt -> showNT nt
  TypeList lt  -> showLT lt
  TypeNonNull nnt -> showNNT nnt

showNT :: NamedType -> Text
showNT = unName . unNamedType

showLT :: ListType -> Text
showLT lt = "[" <> showGT (unListType lt) <> "]"

showNNT :: NonNullType -> Text
showNNT = \case
  NonNullTypeList lt -> showLT lt <> "!"
  NonNullTypeNamed nt -> showNT nt <> "!"

class ToGType a where
  toGT :: a -> GType

class ToNonNullType a where
  toNT :: a -> NonNullType

toLT :: (ToGType a) => a -> ListType
toLT = ListType . toGT

isNotNull :: GType -> Bool
isNotNull (TypeNonNull _) = True
isNotNull _               = False

newtype NamedType
  = NamedType { unNamedType :: Name }
  deriving (Eq, Ord, Show, Hashable, Lift, J.ToJSON,
            J.ToJSONKey, J.FromJSON, J.FromJSONKey)

instance ToGType NamedType where
  toGT = TypeNamed

instance ToNonNullType NamedType where
  toNT = NonNullTypeNamed

newtype ListType
  = ListType {unListType :: GType }
  deriving (Eq, Ord, Show, Lift)

instance ToGType ListType where
  toGT = TypeList

instance ToNonNullType ListType where
  toNT = NonNullTypeList

data NonNullType
  = NonNullTypeNamed NamedType
  | NonNullTypeList  ListType
  deriving (Eq, Ord, Show, Lift)

instance ToGType NonNullType where
  toGT = TypeNonNull

-- * Type definition

data TypeDefinition
  = TypeDefinitionScalar ScalarTypeDefinition
  | TypeDefinitionObject ObjectTypeDefinition
  | TypeDefinitionInterface InterfaceTypeDefinition
  | TypeDefinitionUnion UnionTypeDefinition
  | TypeDefinitionEnum EnumTypeDefinition
  | TypeDefinitionInputObject InputObjectTypeDefinition
  deriving (Show, Eq, Lift)

newtype Description
  = Description { unDescription :: Text }
  deriving (Show, Eq, Ord, IsString, Lift, Semigroup, Monoid)

data ObjectTypeDefinition
  = ObjectTypeDefinition
  { _otdDescription          :: !(Maybe Description)
  , _otdName                 :: !Name
  , _otdImplementsInterfaces :: ![NamedType]
  , _otdDirectives           :: ![Directive]
  , _otdFieldsDefinition     :: ![FieldDefinition]
  }
  deriving (Show, Eq, Lift)

data FieldDefinition
  = FieldDefinition
  { _fldDescription         :: !(Maybe Description)
  , _fldName                :: !Name
  , _fldArgumentsDefinition :: !ArgumentsDefinition
  , _fldType                :: !GType
  , _fldDirectives          :: ![Directive]
  }
  deriving (Show, Eq, Lift)

type ArgumentsDefinition = [InputValueDefinition]

data InputValueDefinition
  = InputValueDefinition
  { _ivdDescription  :: !(Maybe Description)
  , _ivdName         :: !Name
  , _ivdType         :: !GType
  , _ivdDefaultValue :: !(Maybe DefaultValue)
  }
  deriving (Show, Eq, Lift)

data InterfaceTypeDefinition
  = InterfaceTypeDefinition
  { _itdDescription      :: !(Maybe Description)
  , _itdName             :: !Name
  , _itdDirectives       :: ![Directive]
  , _itdFieldsDefinition :: ![FieldDefinition]
  }
  deriving (Show, Eq, Lift)

data UnionTypeDefinition
  = UnionTypeDefinition
  { _utdDescription :: !(Maybe Description)
  , _utdName        :: !Name
  , _utdDirectives  :: ![Directive]
  , _utdMemberTypes :: ![NamedType]
  }
  deriving (Show, Eq, Lift)

data ScalarTypeDefinition
  = ScalarTypeDefinition
  { _stdDescription :: !(Maybe Description)
  , _stdName        :: !Name
  , _stdDirectives  :: ![Directive]
  }
  deriving (Show, Eq, Lift)

data EnumTypeDefinition
  = EnumTypeDefinition
  { _etdDescription      :: !(Maybe Description)
  , _etdName             :: !Name
  , _etdDirectives       :: ![Directive]
  , _etdValueDefinitions :: ![EnumValueDefinition]
  }
  deriving (Show, Eq, Lift)

data EnumValueDefinition
  = EnumValueDefinition
  { _evdDescription :: !(Maybe Description)
  , _evdName        :: !EnumValue
  , _evdDirectives  :: ![Directive]
  }
  deriving (Show, Eq, Lift)

newtype EnumValue
  = EnumValue { unEnumValue :: Name }
  deriving (Show, Eq, Lift, Hashable, J.ToJSON, J.FromJSON, Ord)

data InputObjectTypeDefinition
  = InputObjectTypeDefinition
  { _iotdDescription      :: !(Maybe Description)
  , _iotdName             :: !Name
  , _iotdDirectives       :: ![Directive]
  , _iotdValueDefinitions :: ![InputValueDefinition]
  }
  deriving (Show, Eq, Lift)

data DirectiveDefinition
  = DirectiveDefinition
  { _ddDescription :: !(Maybe Description)
  , _ddName        :: !Name
  , _ddArguments   :: !ArgumentsDefinition
  , _ddLocations   :: ![DirectiveLocation]
  } deriving (Show, Eq, Lift)

data DirectiveLocation
  = DLExecutable !ExecutableDirectiveLocation
  | DLTypeSystem !TypeSystemDirectiveLocation
  deriving (Show, Eq, Lift)

data ExecutableDirectiveLocation
  = EDLQUERY
  | EDLMUTATION
  | EDLSUBSCRIPTION
  | EDLFIELD
  | EDLFRAGMENT_DEFINITION
  | EDLFRAGMENT_SPREAD
  | EDLINLINE_FRAGMENT
  deriving (Show, Eq, Lift)

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
  deriving (Show, Eq, Lift)
