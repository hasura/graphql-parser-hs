{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.GraphQL.Draft.Printer.TextBuilder where

import           Data.Text                     (Text)
import           Protolude
import           Text.Builder

import           Language.GraphQL.Draft.Syntax


renderExecutableDoc :: ExecutableDocument -> Text
renderExecutableDoc = render executableDocument

render :: (a -> Builder) -> a -> Text
render f = run . f

executableDocument :: ExecutableDocument -> Builder
executableDocument ed =
  mconcat $ intersperse (char '\n') $ map executableDefinition $
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
  operationType (_todType op) <> char ' ' <> node op

operationType :: OperationType -> Builder
operationType = \case
  OperationTypeQuery        -> text "query"
  OperationTypeMutation     -> text "mutation"
  OperationTypeSubscription -> text "subscription"

-- TODO: add horizontal nesting
node :: TypedOperationDefinition -> Builder
node (TypedOperationDefinition _ name vars dirs sels) =
  nameB (fromMaybe "" name)
  <> optempty variableDefinitions vars
  <> optempty directives dirs
  <> char ' '
  <> selectionSet sels

-- TODO: add horizontal nesting
selectionSet :: SelectionSet -> Builder
selectionSet [] = mempty
selectionSet xs =
  mconcat [ char '{'
          , char ' '
          , mconcat $ intersperse (char ' ') $ map selection xs
          , char ' '
          , char '}'
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
  <> char ' '
  <> selectionSet selSets

optAlias :: Maybe Alias -> Builder
optAlias = maybe mempty (\(Alias a) -> nameB a <> text ": ")

fragmentSpread :: FragmentSpread -> Builder
fragmentSpread (FragmentSpread name ds) =
  "..." <> nameB name <> optempty directives ds

inlineFragment :: InlineFragment -> Builder
inlineFragment (InlineFragment tc ds sels) =
  text "... "
  <> bool "" "on " (isJust tc)
  <> nameB (fold $ fmap unNamedType tc)
  <> optempty directives ds
  <> selectionSet sels

fragmentDefinition :: FragmentDefinition -> Builder
fragmentDefinition (FragmentDefinition name tc dirs sels) =
  text "fragment "
  <> nameB name
  <> text " on "
  <> nameB (unNamedType tc)
  <> optempty directives dirs
  <> selectionSet sels

directives :: [Directive] -> Builder
directives = mconcat . intersperse (char ' ') . map directive

directive :: Directive -> Builder
directive (Directive name args) =
  char '@' <> nameB name <> optempty arguments args

arguments :: [Argument] -> Builder
arguments xs = char '(' <> args <> char ')'
  where args = mconcat $ intersperse (char ',') $ map argument xs

argument :: Argument -> Builder
argument (Argument name val) = nameB name <> text ": " <> value val

variableDefinitions :: [VariableDefinition] -> Builder
variableDefinitions vars = mconcat [ char '('
                                   , mconcat vars'
                                   , char ')'
                                   ]
  where vars' = intersperse (char ',') $ map variableDefinition vars

variableDefinition :: VariableDefinition -> Builder
variableDefinition (VariableDefinition var ty defVal) =
  variable var <> text ": " <> type_ ty <> maybe "" defaultValue defVal

defaultValue :: DefaultValue -> Builder
defaultValue v = text " = " <> valueC v


-- | Type Reference

type_ :: GType -> Builder
type_ (TypeNamed n x) = nameB (unNamedType x) <> nonNull n
type_ (TypeList  n x) = listType x <> nonNull n

listType :: ListType -> Builder
listType (ListType ty) = char '[' <> type_ ty <> char ']'

nonNull :: Nullability -> Builder
nonNull n = bool (char '!') mempty $ unNullability n

-- | Primitives

value :: Value -> Builder
value = \case
  VVariable v -> variable v
  VInt i      -> decimal i
  -- TODO: fixedDouble? is there any other function that we can use?
  VFloat d    -> fixedDouble 256 d
  VString s   -> stringValue s
  VBoolean b  -> fromBool b
  VNull       -> text "null"
  VList xs    -> listValue xs
  VObject o   -> objectValue o
  VEnum ev    -> nameB $ unEnumValue ev

stringValue :: StringValue -> Builder
stringValue (StringValue s) =
  mconcat [ char '"', text s, char '"' ]

variable :: Variable -> Builder
variable (Variable v) = char '$' <> nameB v

listValue :: ListValue -> Builder
listValue (ListValueG xs) = mconcat [ char '[' , li , char ']' ]
  where
    li = mconcat $ intersperse (char ',') $ map value xs

objectValue :: ObjectValue -> Builder
objectValue (ObjectValueG o) = mconcat [ char '{', vals, char '}' ]
  where
    vals = mconcat $ intersperse (char ',') $ map objectField o

objectField :: ObjectField -> Builder
objectField (ObjectFieldG name val) =
  mconcat [ nameB name, text ": ", value val ]

valueC :: ValueConst -> Builder
valueC = \case
  VCInt i      -> decimal i
  -- TODO: fixedDouble? is there any other function that we can use?
  VCFloat d    -> fixedDouble 256 d
  VCString s   -> stringValue s
  VCBoolean b  -> fromBool b
  VCNull       -> text "null"
  VCList xs    -> listValueC xs
  VCObject o   -> objectValueC o
  VCEnum ev    -> nameB $ unEnumValue ev

listValueC :: ListValueC -> Builder
listValueC (ListValueG xs) = mconcat [ char '[' , li , char ']' ]
  where
    li = mconcat $ intersperse (char ',') $ map valueC xs

objectValueC :: ObjectValueC -> Builder
objectValueC (ObjectValueG o) = mconcat [ char '{', vals, char '}' ]
  where
    vals = mconcat $ intersperse (char ',') $ map objectFieldC o

objectFieldC :: ObjectFieldC -> Builder
objectFieldC (ObjectFieldG name val) =
  nameB name <> text ": " <> valueC val

nameB :: Name -> Builder
nameB (Name n) = text n

fromBool :: Bool -> Builder
fromBool True  = text "true"
fromBool False = text "false"

optempty :: (Eq a, Monoid a, Monoid b) => (a -> b) -> a -> b
optempty f xs = if xs == mempty then mempty else f xs
