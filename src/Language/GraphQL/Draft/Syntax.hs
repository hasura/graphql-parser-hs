{-# LANGUAGE TemplateHaskell      #-}

-- | Description: The GraphQL AST
module Language.GraphQL.Draft.Syntax (
  -- * Basics
    Name
  , unName
  , mkName
  , unsafeMkName
  , litName
  , Description(..)
  , Origin(..)
  , Value(..)
  , literal
  , EnumValue(..)
  , Directive(..)

  -- * Types
  , GType(..)
  , getBaseType
  , Nullability(..)
  , showGT
  , showLT
  , isNullable
  , isNotNull
  , isListType

  -- * Documents
  , Document(..)
  , ExecutableDocument(..)
  , SchemaDocument(..)
  , SchemaIntrospection(..)

  -- * Definitions
  , Definition(..)
  , DirectiveDefinition(..)
  , DirectiveLocation(..)

  -- ** Type system definitions
  , TypeSystemDefinition(..)
  , SchemaDefinition(..)
  , RootOperationTypeDefinition(..)
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
  , InputObjectTypeDefinition(..)
  , TypeSystemDirectiveLocation(..)

  -- ** Executable definitions
  , ExecutableDefinition(..)
  , partitionExDefs
  , OperationDefinition(..)
  , OperationType(..)
  , TypedOperationDefinition(..)
  , VariableDefinition(..)
  , ExecutableDirectiveLocation(..)
  , FragmentDefinition(..)

  -- * Queries
  , SelectionSet
  , Selection(..)
  , Field(..)
  , FragmentSpread(..)
  , NoFragments
  , InlineFragment(..)

  -- ** Fragment conversion functions
  , inline
  , fmapFieldFragment
  , fmapSelectionSetFragment
  , fmapSelectionFragment
  , fmapInlineFragment
  ) where

import qualified Data.Aeson                          as J
import qualified Data.ByteString.Lazy                as BL
import qualified Data.HashMap.Strict                 as M
import qualified Data.Text                           as T
import qualified Language.Haskell.TH.Syntax          as TH
import qualified Text.Regex.TDFA                     as TDFA

import           Control.Monad
import           Data.Bool                           (bool)
import           Data.Hashable
import           Data.HashMap.Strict                 (HashMap)
import           Data.Int                            (Int32)
import           Data.String                         (IsString (..))
import           Data.Text                           (Text)
import           Data.Void
import           GHC.Generics                        (Generic)
import           Instances.TH.Lift                   ()
import           Language.Haskell.TH.Syntax          (Lift, Q)

import {-# SOURCE #-} Language.GraphQL.Draft.Parser       (parseExecutableDoc)
import {-# SOURCE #-} Language.GraphQL.Draft.Printer.Text (renderExecutableDoc)

newtype Name = Name { unName :: Text }
  deriving (Eq, Ord, Show, Hashable, Lift, Semigroup, J.ToJSONKey, J.ToJSON)

-- | Ref: http://facebook.github.io/graphql/June2018/#sec-Names
mkName :: Text -> Maybe Name
mkName text
  | TDFA.match compiledRegex $ T.unpack text = Just (Name text)
  | otherwise                                = Nothing
  where
    compiledRegex = TDFA.makeRegex ("^[_a-zA-Z][_a-zA-Z0-9]*$" :: BL.ByteString) :: TDFA.Regex

unsafeMkName :: Text -> Name
unsafeMkName = Name

parseName :: MonadFail m => Text -> m Name
parseName text = maybe (fail errorMessage) pure $ mkName text
  where errorMessage = T.unpack text <> " is not valid GraphQL name"

litName :: Text -> Q (TH.TExp Name)
litName = parseName >=> \name -> [|| name ||]

instance J.FromJSON Name where
  parseJSON = J.withText "Name" parseName

instance J.FromJSONKey Name where
  fromJSONKey = J.FromJSONKeyTextParser parseName

-- * Documents

newtype Document
  = Document { getDefinitions :: [Definition] }
  deriving (Ord, Show, Eq, Lift)

data Definition
  = DefinitionExecutable (ExecutableDefinition Name)
  | DefinitionTypeSystem TypeSystemDefinition
  deriving (Ord, Show, Eq, Lift, Generic)
instance Hashable Definition

newtype ExecutableDocument var
  = ExecutableDocument { getExecutableDefinitions :: [ExecutableDefinition var] }
  deriving (Ord, Show, Eq, Lift, Hashable, Functor, Foldable, Traversable)

instance J.FromJSON (ExecutableDocument Name) where
  parseJSON = J.withText "ExecutableDocument" $ \t ->
    case parseExecutableDoc t of
      Right a -> return a
      Left _  -> fail "parsing the graphql query failed"

instance J.ToJSON (ExecutableDocument Name) where
  toJSON = J.String . renderExecutableDoc

data ExecutableDefinition var
  = ExecutableDefinitionOperation (OperationDefinition FragmentSpread var)
  | ExecutableDefinitionFragment FragmentDefinition
  deriving (Ord, Show, Eq, Lift, Functor, Foldable, Traversable, Generic)
instance Hashable var => Hashable (ExecutableDefinition var)

partitionExDefs
  :: [ExecutableDefinition var]
  -> ( [SelectionSet FragmentSpread var]
     , [TypedOperationDefinition FragmentSpread var]
     , [FragmentDefinition] )
partitionExDefs = foldr f ([], [], [])
  where
    f d (selSets, ops, frags) = case d of
      ExecutableDefinitionOperation (OperationDefinitionUnTyped t) ->
        (t:selSets, ops, frags)
      ExecutableDefinitionOperation (OperationDefinitionTyped t) ->
        (selSets, t:ops, frags)
      ExecutableDefinitionFragment frag ->
        (selSets, ops, frag:frags)

data TypeSystemDefinition
  = TypeSystemDefinitionSchema SchemaDefinition
  | TypeSystemDefinitionType (TypeDefinition ())
  deriving (Ord, Show, Eq, Lift, Generic)

instance Hashable TypeSystemDefinition

data SchemaDefinition = SchemaDefinition
  { _sdDirectives                   :: Maybe [Directive Void]
  , _sdRootOperationTypeDefinitions :: [RootOperationTypeDefinition]
  } deriving (Ord, Show, Eq, Lift, Generic)
instance Hashable SchemaDefinition

data RootOperationTypeDefinition = RootOperationTypeDefinition
  { _rotdOperationType     :: OperationType
  , _rotdOperationTypeType :: Name
  } deriving (Ord, Show, Eq, Lift, Generic)
instance Hashable RootOperationTypeDefinition

data OperationType
  = OperationTypeQuery
  | OperationTypeMutation
  | OperationTypeSubscription
  deriving (Ord, Show, Eq, Lift, Generic)
instance Hashable OperationType

newtype SchemaDocument
  = SchemaDocument [TypeDefinition ()] -- No 'possibleTypes' specified for interfaces
  deriving (Ord, Show, Eq, Lift, Hashable, Generic)

-- | A variant of 'SchemaDocument' that additionally stores, for each interface,
-- the list of object types that implement that interface
newtype SchemaIntrospection
  = SchemaIntrospection [TypeDefinition [Name]]
  deriving (Ord, Show, Eq, Lift, Hashable)

data OperationDefinition frag var
  = OperationDefinitionTyped (TypedOperationDefinition frag var)
  | OperationDefinitionUnTyped (SelectionSet frag var)
  deriving (Ord, Show, Eq, Lift, Functor, Foldable, Traversable, Generic)
instance (Hashable (frag var), Hashable var) => Hashable (OperationDefinition frag var)

data TypedOperationDefinition frag var = TypedOperationDefinition
  { _todType                :: OperationType
  , _todName                :: Maybe Name
  , _todVariableDefinitions :: [VariableDefinition]
  , _todDirectives          :: [Directive var]
  , _todSelectionSet        :: SelectionSet frag var
  } deriving (Ord, Show, Eq, Lift, Functor, Foldable, Traversable, Generic)
instance (Hashable (frag var), Hashable var) => Hashable (TypedOperationDefinition frag var)

data VariableDefinition = VariableDefinition
  { _vdName         :: Name
  , _vdType         :: GType
  , _vdDefaultValue :: Maybe (Value Void)
  } deriving (Ord, Show, Eq, Lift, Generic)
instance Hashable VariableDefinition

type SelectionSet frag var = [Selection frag var]

data Selection frag var
  = SelectionField (Field frag var)
  | SelectionFragmentSpread (frag var)
  | SelectionInlineFragment (InlineFragment frag var)
  deriving (Ord, Show, Eq, Lift, Functor, Foldable, Traversable, Generic)
instance (Hashable (frag var), Hashable var) => Hashable (Selection frag var)

data Field frag var = Field
  { _fAlias        :: Maybe Name
  , _fName         :: Name
  , _fArguments    :: HashMap Name (Value var)
  , _fDirectives   :: [Directive var]
  , _fSelectionSet :: SelectionSet frag var
  } deriving (Ord, Show, Eq, Functor, Foldable, Traversable, Generic)
instance (Hashable (frag var), Hashable var) => Hashable (Field frag var)
instance (Lift (frag var), Lift var) => Lift (Field frag var) where
  liftTyped Field{..} =
    [|| Field { _fAlias, _fName, _fDirectives, _fSelectionSet
             , _fArguments = $$(liftTypedHashMap _fArguments) } ||]

-- * Fragments

data FragmentSpread var = FragmentSpread
  { _fsName       :: Name
  , _fsDirectives :: [Directive var]
  } deriving (Ord, Show, Eq, Lift, Functor, Foldable, Traversable, Generic)
instance Hashable var => Hashable (FragmentSpread var)

-- | Can be used in place of the @frag@ parameter to various AST types to
-- guarante that the AST does not include any fragment spreads.
--
-- Note: This is equivalent to @'Const' 'Void'@, but annoyingly, 'Const' does
-- not provide a 'Lift' instance as of GHC 8.6.
data NoFragments var
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Lift, Generic)
instance Hashable (NoFragments var)

data InlineFragment frag var = InlineFragment
  { _ifTypeCondition :: Maybe Name
  , _ifDirectives    :: [Directive var]
  , _ifSelectionSet  :: SelectionSet frag var
  } deriving (Ord, Show, Eq, Lift, Functor, Foldable, Traversable, Generic)
instance (Hashable (frag var), Hashable var) => Hashable (InlineFragment frag var)

data FragmentDefinition = FragmentDefinition
  { _fdName          :: Name
  , _fdTypeCondition :: Name
  , _fdDirectives    :: [Directive Void]
  , _fdSelectionSet  :: SelectionSet FragmentSpread Void
  } deriving (Ord, Show, Eq, Lift, Generic)
instance Hashable FragmentDefinition

-- * Values

data Origin = GraphQLLiteral
            | ExternalValue
  deriving (Show, Ord, Eq, Lift, Generic)

instance Hashable Origin

data Value var
  = VVariable var
  | VNull
  | VInt Int32
  | VFloat Double
  | VString Origin Text -- see note [The origin of VString]
  | VBoolean Bool
  | VEnum EnumValue
  | VList [Value var]
  | VObject (HashMap Name (Value var))
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)
instance Hashable var => Hashable (Value var)
instance Lift var => Lift (Value var) where
  liftTyped (VVariable a) = [|| VVariable a ||]
  liftTyped VNull         = [|| VNull ||]
  liftTyped (VInt a)      = [|| VInt a ||]
  liftTyped (VFloat a)    = [|| VFloat a ||]
  liftTyped (VString o a) = [|| VString o a ||]
  liftTyped (VBoolean a)  = [|| VBoolean a ||]
  liftTyped (VEnum a)     = [|| VEnum a ||]
  liftTyped (VList a)     = [|| VList a ||]
  liftTyped (VObject a)   = [|| VObject $$(liftTypedHashMap a) ||]

literal :: Value Void -> Value var
literal = fmap absurd

{- Note [The origin of VSTring]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

All abstractions are leaky. And, sometimes, the leak manifests itself quite far
from where it originated. Here, the reason why we need to know where a VString
came from has to do with how we parse and typecheck enum values in an incoming
query in Hasura's graphql server.



To understand why, let's start with the GraphQL spec. The spec lists, for each
input type, what implicit conversion / coercion can be performed. An int literal
can be silently promoted to float, for instance. More subtle: if a list of a
given type T is expected, it is allowed to only give one value of said type T,
which will then be promoted to a list of one element. And finally, the relevant
point: an Enum value CANNOT be coerced from a String. To quote section 3.9:

    GraphQL has a constant literal to represent enum input values. GraphQL
    string literals must not be accepted as an enum input and instead raise a
    query error.

    Query variable transport serializations which have a different
    representation for non-string symbolic values (for example, EDN) should only
    allow such values as enum input values. Otherwise, for most transport
    serializations that do not, strings may be interpreted as the enum input
    value with the same name.

This in itself is enough to summarize why we need the origin of the VString: a
conversion is allowed from String to Enum value IF AND ONLY IF said String
originated from a serialization that does not differentiate between strings and
enum values, like JSON.



But why would our parsers need to convert from a JSON value to a 'Value' in the
first place? Why can't they decide whether a JSON string is an enum value or a
string based on the context in which it is interpreted? To answer that, let's
talk about our parsers.

Inside our parsing logic, all parsers for an input value expect a 'Value'. They
can there check that the types match, and perform any required input
coercion. For instance, the parser for a float could look like this:

    float :: Parser Double
    float = \case
      VInt   i -> pure $ fromIntegral i
      VFLoat f -> pure f
      other    -> typeMismatch "Float" $ describe other

For more complicated types, we combine together smaller parsers. An extremely
important property of those parsers is that they don't check how a type should
be structured by looking it up in an external source of truth: THEY ARE THE
SOURCE OF TRUTH. They are made in a way that can be statically introspected, and
the definition of a type can be extracted from how we parse it.

What that means, in turn, is that the place where we can tell that we are
expecting an enum value, and that we should parse an incoming JSON string as an
emum is in the `enum` combinator.

Finally, the last piece of the puzzle: variable substitution. 'Value' is
polymorphic over variables: in practice, the type we use for variables contains
a `Value Void`. When we perform variable substitution, we simply replace the
'Value' by the one inside the variable.

With all of that in mind, let's see how else we could solve the problem of
making sure JSON strings can be properly converted into enums.



Solution 1: type-aware literal conversion

The easiest way to deal with JSON values, and indeed the one on which our
current solution is based, is to convert them into 'Value's before parsing
begins. But each JSON string on the inside would need to be checked against the
schema, to know whether it should be translated into a 'VString' or a
'VEnumValue'. Let's take a contrived example: a variable defined as such:

    x: {
      "foo": {
        "bar": {
          "baz": "RED"
        }
      }
    }

To check whether this "RED" string should be interpreted as an enum value or
not, we would need to identify the type, of all fields in all objects all the
way up, while also performing some of the same input type coercion checks that
the parsers already have to do: maybe the field "bar" actually expects a list of
values, but as we've seen it is legal to only provide one value for a list.

We would basically have to duplicate / rewrite a significant (and, crucically, a
delicate and error-prone) part of the parsers logic just to convert JSON values
to GraphQL 'Value's.



Solution 2: JSON values inside the variable

Another approach would have been to change the type we use for variables, the
type argument to 'Value', and for it to store either a GraphQL value, or an
uninterpreted JSON value. With this, each leaf parser could decide how to deal
with the underlying JSON value, and decide whether a String should be allowed to
be cast to enum.

Where this approach failed is for variable substitution. Since we're still using
Value as the input to our parser, we would need to convert the JSON value into a
GraphQL 'Value' upon performing variable substitution. But since we would not be
in the leaf parsers, we would have to, again, introspect the schema to know how
to handle the strings, same problem as solution 1.



Solution 3: changing the parsers input

The next logical step was to change the input of our parsers to be instead a new
union type:

    data InputValue a = JSONValue Aeson.Value
                      | GraphQLValue (Value a)

    data Variable = Variable (InputValue Void)

    type ParserInput = InputValue Variable

That way, when performing variable substitution, we could promote the variable's
JSON value to the level of InputValue. But... this wasn't over. Consider the
case of the 'object' combinator, finding a JSON value. If it tries to convert
the entire object, then we run into the same issue again: it will need knowledge
of the entire object, down to the leaves, which means the same problem as the
two previous solutions.

This could be solved by only lazily converting lists and objects... But in their
current implementation, 'VObject' and 'VList' expect values of type
'Value'. Those would need to be made parametric too... and that would break
everything.



As a result, there was no way to fix this issue without either deeply changing
how some parts of the parsers work, or without fundamentally chaning the 'Value'
type. Hence this solution to simply tag the 'VString', to indicate whether it
originated from a GraphQL literal or if it was converted from another format.

-}


-- * Directives

data Directive var = Directive
  { _dName      :: Name
  , _dArguments :: HashMap Name (Value var)
  } deriving (Ord, Show, Eq, Functor, Foldable, Traversable, Generic)
instance Hashable var => Hashable (Directive var)
instance Lift var => Lift (Directive var) where
  liftTyped Directive{..} = [|| Directive{ _dName, _dArguments = $$(liftTypedHashMap _dArguments) } ||]

-- * Type Reference

newtype Nullability
  = Nullability { unNullability :: Bool }
  deriving (Show, Ord, Eq, Lift, Generic, Hashable)

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

showGT :: GType -> Text
showGT = \case
  TypeNamed nullability nt -> unName nt <> showNullable nullability
  TypeList nullability lt  -> showLT lt <> showNullable nullability

showNullable :: Nullability -> Text
showNullable = bool "!" "" . unNullability

showLT :: GType -> Text
showLT lt = "[" <> showGT lt <> "]"

isNullable :: GType -> Bool
isNullable = \case
  TypeNamed nullability _ -> unNullability nullability
  TypeList nullability _  -> unNullability nullability

isListType :: GType -> Bool
isListType = \case
  TypeList _ _  -> True
  TypeNamed _ _ -> False

isNotNull :: GType -> Bool
isNotNull = not . isNullable

-- * Type definition

data TypeDefinition possibleTypes
  = TypeDefinitionScalar ScalarTypeDefinition
  | TypeDefinitionObject ObjectTypeDefinition
  | TypeDefinitionInterface (InterfaceTypeDefinition possibleTypes)
  | TypeDefinitionUnion UnionTypeDefinition
  | TypeDefinitionEnum EnumTypeDefinition
  | TypeDefinitionInputObject InputObjectTypeDefinition
  deriving (Ord, Show, Eq, Lift, Generic)
instance Hashable possibleTypes => Hashable (TypeDefinition possibleTypes)

newtype Description
  = Description { unDescription :: Text }
  deriving (Show, Eq, Ord, IsString, Lift, Semigroup, Monoid, Hashable, J.ToJSON, J.FromJSON)

data ObjectTypeDefinition = ObjectTypeDefinition
  { _otdDescription          :: (Maybe Description)
  , _otdName                 :: Name
  , _otdImplementsInterfaces :: [Name]
  , _otdDirectives           :: [Directive Void]
  , _otdFieldsDefinition     :: [FieldDefinition]
  } deriving (Ord, Show, Eq, Lift, Generic)
instance Hashable ObjectTypeDefinition

data FieldDefinition = FieldDefinition
  { _fldDescription         :: Maybe Description
  , _fldName                :: Name
  , _fldArgumentsDefinition :: ArgumentsDefinition
  , _fldType                :: GType
  , _fldDirectives          :: [Directive Void]
  } deriving (Ord, Show, Eq, Lift, Generic)
instance Hashable FieldDefinition

type ArgumentsDefinition = [InputValueDefinition]

data InputValueDefinition = InputValueDefinition
  { _ivdDescription  :: Maybe Description
  , _ivdName         :: Name
  , _ivdType         :: GType
  , _ivdDefaultValue :: Maybe (Value Void)
  } deriving (Ord, Show, Eq, Lift, Generic)
instance Hashable InputValueDefinition

data InterfaceTypeDefinition possibleTypes = InterfaceTypeDefinition
  { _itdDescription      :: Maybe Description
  , _itdName             :: Name
  , _itdDirectives       :: [Directive Void]
  , _itdFieldsDefinition :: [FieldDefinition]
  , _itdPossibleTypes    :: possibleTypes
  } deriving (Ord, Show, Eq, Lift, Generic)
instance Hashable possibleTypes => Hashable (InterfaceTypeDefinition possibleTypes)

data UnionTypeDefinition = UnionTypeDefinition
  { _utdDescription :: Maybe Description
  , _utdName        :: Name
  , _utdDirectives  :: [Directive Void]
  , _utdMemberTypes :: [Name]
  } deriving (Ord, Show, Eq, Lift, Generic)
instance Hashable UnionTypeDefinition

data ScalarTypeDefinition = ScalarTypeDefinition
  { _stdDescription :: Maybe Description
  , _stdName        :: Name
  , _stdDirectives  :: [Directive Void]
  } deriving (Ord, Show, Eq, Lift, Generic)
instance Hashable ScalarTypeDefinition

data EnumTypeDefinition = EnumTypeDefinition
  { _etdDescription      :: Maybe Description
  , _etdName             :: Name
  , _etdDirectives       :: [Directive Void]
  , _etdValueDefinitions :: [EnumValueDefinition]
  } deriving (Ord, Show, Eq, Lift, Generic)
instance Hashable EnumTypeDefinition

data EnumValueDefinition = EnumValueDefinition
  { _evdDescription :: (Maybe Description)
  , _evdName        :: EnumValue
  , _evdDirectives  :: [Directive Void]
  } deriving (Ord, Show, Eq, Lift, Generic)
instance Hashable EnumValueDefinition

newtype EnumValue
  = EnumValue { unEnumValue :: Name }
  deriving (Show, Eq, Lift, Hashable, J.ToJSON, J.FromJSON, Ord)

data InputObjectTypeDefinition = InputObjectTypeDefinition
  { _iotdDescription      :: Maybe Description
  , _iotdName             :: Name
  , _iotdDirectives       :: [Directive Void]
  , _iotdValueDefinitions :: [InputValueDefinition]
  } deriving (Ord, Show, Eq, Lift, Generic)
instance Hashable InputObjectTypeDefinition

data DirectiveDefinition = DirectiveDefinition
  { _ddDescription :: (Maybe Description)
  , _ddName        :: Name
  , _ddArguments   :: ArgumentsDefinition
  , _ddLocations   :: [DirectiveLocation]
  } deriving (Ord, Show, Eq, Lift, Generic)
instance Hashable DirectiveDefinition

data DirectiveLocation
  = DLExecutable ExecutableDirectiveLocation
  | DLTypeSystem TypeSystemDirectiveLocation
  deriving (Ord, Show, Eq, Lift, Generic)
instance Hashable DirectiveLocation

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

liftTypedHashMap :: (Eq k, Hashable k, Lift k, Lift v) => HashMap k v -> Q (TH.TExp (HashMap k v))
liftTypedHashMap a = [|| M.fromList $$(TH.liftTyped $ M.toList a) ||]

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

fmapInlineFragment :: (frag var -> frag' var) -> (InlineFragment frag var) -> (InlineFragment frag' var)
fmapInlineFragment f inlineFragment =
  inlineFragment {_ifSelectionSet = fmapSelectionSetFragment f (_ifSelectionSet inlineFragment)}
