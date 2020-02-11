{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Language.GraphQL.Draft.Printer where

import           Prelude                       (String)
import           Protolude

import           Language.GraphQL.Draft.Syntax


class (Monoid a, IsString a) => Printer a where
  stringP  :: String -> a
  textP    :: Text -> a
  charP    :: Char -> a
  intP     :: Int32 -> a
  doubleP  :: Double -> a

  {-# MINIMAL stringP, textP, charP, intP, doubleP #-}

  nameP    :: Name -> a
  nameP    = textP . unName

  nodeP :: TypedOperationDefinition -> a
  nodeP = node

  selectionSetP :: SelectionSet -> a
  selectionSetP = selectionSet


-- | the pretty printer implementation

executableDocument :: (Printer a) => ExecutableDocument -> a
executableDocument ed =
  mconcat $ intersperse (charP '\n') $ map executableDefinition $
  getExecutableDefinitions ed

executableDefinition :: (Printer a) => ExecutableDefinition -> a
executableDefinition = \case
  ExecutableDefinitionOperation d -> operationDefinition d
  ExecutableDefinitionFragment  d -> fragmentDefinition d

operationDefinition :: (Printer a) => OperationDefinition -> a
operationDefinition = \case
  OperationDefinitionUnTyped selSet -> selectionSetP selSet
  OperationDefinitionTyped op       -> typedOperationDefinition op

typedOperationDefinition :: (Printer a) => TypedOperationDefinition -> a
typedOperationDefinition op =
  operationType (_todType op) <> charP ' ' <> nodeP op

operationType :: (Printer a) => OperationType -> a
operationType = \case
  OperationTypeQuery        -> "query"
  OperationTypeMutation     -> "mutation"
  OperationTypeSubscription -> "subscription"

-- TODO: add horizontal nesting
node :: (Printer a) => TypedOperationDefinition -> a
node (TypedOperationDefinition _ name vars dirs sels) =
  nameP (fromMaybe "" name)
  <> optempty variableDefinitions vars
  <> optempty directives dirs
  <> charP ' '
  <> selectionSetP sels

-- TODO: add horizontal nesting
selectionSet :: (Printer a) => SelectionSet -> a
selectionSet [] = mempty
selectionSet xs =
  "{ " <> mconcat (intersperse (charP ' ') (map selection xs)) <> " }"

selection :: (Printer a) => Selection -> a
selection = \case
  SelectionField fld          -> field fld
  SelectionFragmentSpread fs  -> fragmentSpread fs
  SelectionInlineFragment ilf -> inlineFragment ilf

field :: (Printer a) => Field -> a
field (Field alias name args dirs selSets) =
  optAlias alias
  <> nameP name
  <> optempty arguments args
  <> optempty directives dirs
  <> charP ' '
  <> selectionSetP selSets

optAlias :: (Printer a) => Maybe Alias -> a
optAlias = maybe mempty (\(Alias a) -> nameP a <> textP ": ")

fragmentSpread :: (Printer a) => FragmentSpread -> a
fragmentSpread (FragmentSpread name ds) =
  "..." <> nameP name <> optempty directives ds

inlineFragment :: (Printer a) => InlineFragment -> a
inlineFragment (InlineFragment tc ds sels) =
  "... "
  <> bool mempty (textP "on") (isJust tc)
  <> nameP (fold $ fmap unNamedType tc)
  <> optempty directives ds
  <> selectionSetP sels

fragmentDefinition :: (Printer a) => FragmentDefinition -> a
fragmentDefinition (FragmentDefinition name tc dirs sels) =
  "fragment "
  <> nameP name
  <> " on "
  <> nameP (unNamedType tc)
  <> optempty directives dirs
  <> selectionSetP sels

directives :: (Printer a) => [Directive] -> a
directives = mconcat . intersperse (charP ' ') . map directive

directive :: (Printer a) => Directive -> a
directive (Directive name args) =
  charP '@' <> nameP name <> optempty arguments args

arguments :: (Printer a) => [Argument] -> a
arguments xs = charP '(' <> args <> charP ')'
  where args = mconcat $ intersperse (charP ',') $ map argument xs

argument :: (Printer a) => Argument -> a
argument (Argument name val) = nameP name <> ": " <> value val

variableDefinitions :: (Printer a) => [VariableDefinition] -> a
variableDefinitions vars = mconcat [ charP '('
                                   , mconcat vars'
                                   , charP ')'
                                   ]
  where vars' = intersperse (charP ',') $ map variableDefinition vars

variableDefinition :: (Printer a) => VariableDefinition -> a
variableDefinition (VariableDefinition var ty defVal) =
  variable var <> ": " <> graphQLType ty <> maybe mempty defaultValue defVal

defaultValue :: (Printer a) => DefaultValue -> a
defaultValue v = " =" <> charP ' ' <> valueC v

-- | Type Reference

graphQLType :: (Printer a) => GType -> a
graphQLType (TypeNamed n x) = nameP (unNamedType x) <> nonNull n
graphQLType (TypeList  n x) = listType x <> nonNull n

listType :: (Printer a) => ListType -> a
listType (ListType ty) = charP '[' <> graphQLType ty <> charP ']'

nonNull :: (Printer a) => Nullability -> a
nonNull n = bool (charP '!') mempty $ unNullability n

-- | Primitives

value :: (Printer a) => Value -> a
value = \case
  VVariable v -> variable v
  VInt i      -> intP i
  VFloat d    -> doubleP d
  VString s   -> stringValue s
  VBoolean b  -> fromBool b
  VNull       -> "null"
  VList xs    -> listValue xs
  VObject o   -> objectValue o
  VEnum ev    -> nameP $ unEnumValue ev

stringValue :: (Printer a) => StringValue -> a
stringValue (StringValue s) =
  mconcat [ charP '"', textP s, charP '"' ]

variable :: (Printer a) => Variable -> a
variable (Variable v) = charP '$' <> nameP v

listValue :: (Printer a) => ListValue -> a
listValue (ListValueG xs) = mconcat [ charP '[' , li , charP ']' ]
  where
    li = mconcat $ intersperse (charP ',') $ map value xs

objectValue :: (Printer a) => ObjectValue -> a
objectValue (ObjectValueG o) = mconcat [ charP '{', vals, charP '}' ]
  where
    vals = mconcat $ intersperse (charP ',') $ map objectField o

objectField :: (Printer a) => ObjectField -> a
objectField (ObjectFieldG name val) =
  mconcat [ nameP name, charP ':', value val ]

valueC :: (Printer a) => ValueConst -> a
valueC = \case
  VCInt i      -> intP i
  VCFloat d    -> doubleP d
  VCString s   -> stringValue s
  VCBoolean b  -> fromBool b
  VCNull       -> "null"
  VCList xs    -> listValueC xs
  VCObject o   -> objectValueC o
  VCEnum ev    -> nameP $ unEnumValue ev

listValueC :: (Printer a) => ListValueC -> a
listValueC (ListValueG xs) = mconcat [ charP '[' , li , charP ']' ]
  where
    li = mconcat $ intersperse (charP ',') $ map valueC xs

objectValueC :: (Printer a) => ObjectValueC -> a
objectValueC (ObjectValueG o) = mconcat [ charP '{', vals, charP '}' ]
  where
    vals = mconcat $ intersperse (charP ',') $ map objectFieldC o

objectFieldC :: (Printer a) => ObjectFieldC -> a
objectFieldC (ObjectFieldG name val) =
  nameP name <> ": " <> valueC val

fromBool :: (Printer a) => Bool -> a
fromBool True  = "true"
fromBool False = "false"

optempty :: (Eq a, Monoid a, Monoid b) => (a -> b) -> a -> b
optempty f xs = if xs == mempty then mempty else f xs
