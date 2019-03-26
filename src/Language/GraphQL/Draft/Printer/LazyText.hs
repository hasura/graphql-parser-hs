{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.GraphQL.Draft.Printer.LazyText where

import           Data.Text.Lazy                (Text)
import           Data.Text.Lazy.Builder
import           Protolude                     hiding (Text)

import           Language.GraphQL.Draft.Syntax


renderExecutableDoc :: ExecutableDocument -> Text
renderExecutableDoc = render executableDocument

render :: (a -> Builder) -> a -> Text
render f v = toLazyText $ f v

executableDocument :: ExecutableDocument -> Builder
executableDocument ed =
  mconcat $ intersperse (singleton '\n') $ map executableDefinition $
  getExecutableDefinitions ed

executableDefinition :: ExecutableDefinition -> Builder
executableDefinition = \case
  ExecutableDefinitionOperation d -> operationDefinition d
  ExecutableDefinitionFragment  d -> fragmentDefinition d

operationDefinition :: OperationDefinition -> Builder
operationDefinition = \case
  OperationDefinitionUnTyped selSet -> selectionSet selSet
  OperationDefinitionTyped op       -> typedOperationDefinition op

typedOperationDefinition :: TypedOperationDefinition -> Builder
typedOperationDefinition op =
  operationType (_todType op) <> singleton ' ' <> node op

operationType :: OperationType -> Builder
operationType = \case
  OperationTypeQuery        -> fromText "query"
  OperationTypeMutation     -> fromText "mutation"
  OperationTypeSubscription -> fromText "subscription"

-- TODO: add horizontal nesting
node :: TypedOperationDefinition -> Builder
node (TypedOperationDefinition _ name vars dirs sels) =
  nameB (fromMaybe "" name)
  <> optempty variableDefinitions vars
  <> optempty directives dirs
  <> singleton ' '
  <> selectionSet sels

-- TODO: add horizontal nesting
selectionSet :: SelectionSet -> Builder
selectionSet [] = mempty
selectionSet xs =
  mconcat [ singleton '{'
          , singleton ' '
          , mconcat $ intersperse (singleton ' ') $ map selection xs
          , singleton ' '
          , singleton '}'
          ]

selection :: Selection -> Builder
selection = \case
  SelectionField fld          -> field fld
  SelectionFragmentSpread fs  -> fragmentSpread fs
  SelectionInlineFragment ilf -> inlineFragment ilf

field :: Field -> Builder
field (Field alias name args dirs selSets) =
  optAlias alias
  <> nameB name
  <> optempty arguments args
  <> optempty directives dirs
  <> singleton ' '
  <> selectionSet selSets

optAlias :: Maybe Alias -> Builder
optAlias = maybe mempty (\(Alias a) -> nameB a <> fromText ": ")

fragmentSpread :: FragmentSpread -> Builder
fragmentSpread (FragmentSpread name ds) =
  "..." <> nameB name <> optempty directives ds

inlineFragment :: InlineFragment -> Builder
inlineFragment (InlineFragment tc ds sels) =
  fromText "... "
  <> bool "" "on " (isJust tc)
  <> nameB (fold $ fmap unNamedType tc)
  <> optempty directives ds
  <> selectionSet sels

fragmentDefinition :: FragmentDefinition -> Builder
fragmentDefinition (FragmentDefinition name tc dirs sels) =
  fromText "fragment "
  <> nameB name
  <> fromText " on "
  <> nameB (unNamedType tc)
  <> optempty directives dirs
  <> selectionSet sels

directives :: [Directive] -> Builder
directives = mconcat . intersperse (singleton ' ') . map directive

directive :: Directive -> Builder
directive (Directive name args) =
  singleton '@' <> nameB name <> optempty arguments args

arguments :: [Argument] -> Builder
arguments xs = singleton '(' <> args <> singleton ')'
  where args = mconcat $ intersperse (singleton ',') $ map argument xs

argument :: Argument -> Builder
argument (Argument name val) = nameB name <> fromText ": " <> value val

variableDefinitions :: [VariableDefinition] -> Builder
variableDefinitions vars = mconcat [ singleton '('
                                   , mconcat vars'
                                   , singleton ')'
                                   ]
  where vars' = intersperse (singleton ',') $ map variableDefinition vars

variableDefinition :: VariableDefinition -> Builder
variableDefinition (VariableDefinition var ty defVal) =
  variable var <> fromText ": " <> type_ ty <> maybe "" defaultValue defVal

defaultValue :: DefaultValue -> Builder
defaultValue v = fromText " = " <> valueC v


-- | Type Reference

type_ :: GType -> Builder
type_ (TypeNamed n x) = nameB (unNamedType x) <> nonNull n
type_ (TypeList  n x) = listType x <> nonNull n

listType :: ListType -> Builder
listType (ListType ty) = singleton '[' <> type_ ty <> singleton ']'

nonNull :: Nullability -> Builder
nonNull n = bool (singleton '!') mempty $ unNullability n

-- | Primitives

value :: Value -> Builder
value = \case
  VVariable v -> variable v
  VInt i      -> fromString $ show i
  VFloat d    -> fromString $ show d
  VString s   -> stringValue s
  VBoolean b  -> fromBool b
  VNull       -> fromText "null"
  VList xs    -> listValue xs
  VObject o   -> objectValue o
  VEnum ev    -> nameB $ unEnumValue ev

stringValue :: StringValue -> Builder
stringValue (StringValue s) =
  mconcat [ singleton '"', fromText s, singleton '"' ]

variable :: Variable -> Builder
variable (Variable v) = singleton '$' <> nameB v

listValue :: ListValue -> Builder
listValue (ListValueG xs) = mconcat [ singleton '[' , li , singleton ']' ]
  where
    li = mconcat $ intersperse (singleton ',') $ map value xs

objectValue :: ObjectValue -> Builder
objectValue (ObjectValueG o) = mconcat [ singleton '{', vals, singleton '}' ]
  where
    vals = mconcat $ intersperse (singleton ',') $ map objectField o

objectField :: ObjectField -> Builder
objectField (ObjectFieldG name val) =
  mconcat [ nameB name, fromText ": ", value val ]

valueC :: ValueConst -> Builder
valueC = \case
  VCInt i      -> fromString $ show i
  VCFloat d    -> fromString $ show d
  VCString s   -> stringValue s
  VCBoolean b  -> fromBool b
  VCNull       -> fromText "null"
  VCList xs    -> listValueC xs
  VCObject o   -> objectValueC o
  VCEnum ev    -> nameB $ unEnumValue ev

listValueC :: ListValueC -> Builder
listValueC (ListValueG xs) = mconcat [ singleton '[' , li , singleton ']' ]
  where
    li = mconcat $ intersperse (singleton ',') $ map valueC xs

objectValueC :: ObjectValueC -> Builder
objectValueC (ObjectValueG o) = mconcat [ singleton '{', vals, singleton '}' ]
  where
    vals = mconcat $ intersperse (singleton ',') $ map objectFieldC o

objectFieldC :: ObjectFieldC -> Builder
objectFieldC (ObjectFieldG name val) =
  nameB name <> fromText ": " <> valueC val

nameB :: Name -> Builder
nameB (Name n) = fromText n

fromBool :: Bool -> Builder
fromBool True  = fromText "true"
fromBool False = fromText "false"

optempty :: (Eq a, Monoid a, Monoid b) => (a -> b) -> a -> b
optempty f xs = if xs == mempty then mempty else f xs
