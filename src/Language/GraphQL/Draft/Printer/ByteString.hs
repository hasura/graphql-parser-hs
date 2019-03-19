{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.GraphQL.Draft.Printer.ByteString where

import           Data.ByteString.Builder
import           Data.List                     (intersperse)
import           Protolude

import qualified Data.ByteString.Lazy          as BL
import qualified Data.Text.Lazy                as LT
import qualified Data.Text.Lazy.Encoding       as LT

import           Language.GraphQL.Draft.Syntax


render :: (a -> Builder) -> a -> BL.ByteString
render f = toLazyByteString . f

renderExecutableDoc :: ExecutableDocument -> BL.ByteString
renderExecutableDoc = toLazyByteString . executableDocument

renderSel :: Selection -> BL.ByteString
renderSel = toLazyByteString . selection

renderSelSet :: SelectionSet -> BL.ByteString
renderSelSet = toLazyByteString . selectionSet

executableDocument :: ExecutableDocument -> Builder
executableDocument ed =
  mconcat $ intersperse (charUtf8 '\n') $ map executableDefinition $
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
  operationType (_todType op) <> charUtf8 ' ' <> node op

operationType :: OperationType -> Builder
operationType = \case
  OperationTypeQuery        -> stringUtf8 "query"
  OperationTypeMutation     -> stringUtf8 "mutation"
  OperationTypeSubscription -> stringUtf8 "subscription"

-- TODO: add horizontal nesting
node :: TypedOperationDefinition -> Builder
node (TypedOperationDefinition _ name vars dirs sels) =
  nameB (fromMaybe "" name)
  <> optempty variableDefinitions vars
  <> optempty directives dirs
  <> charUtf8 ' '
  <> selectionSet sels

-- TODO: add horizontal nesting
selectionSet :: SelectionSet -> Builder
selectionSet [] = mempty
selectionSet xs =
  mconcat [ charUtf8 '{'
          , charUtf8 ' '
          , mconcat $ intersperse (charUtf8 ' ') $ map selection xs
          , charUtf8 ' '
          , charUtf8 '}'
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
  <> charUtf8 ' '
  <> selectionSet selSets

optAlias :: Maybe Alias -> Builder
optAlias = maybe mempty (\(Alias a) -> nameB a <> stringUtf8 ": ")

fragmentSpread :: FragmentSpread -> Builder
fragmentSpread (FragmentSpread name ds) =
  "..." <> nameB name <> optempty directives ds

inlineFragment :: InlineFragment -> Builder
inlineFragment (InlineFragment tc ds sels) =
  stringUtf8 "... "
  <> bool "" "on" (isJust tc)
  <> nameB (fold $ fmap unNamedType tc)
  <> optempty directives ds
  <> selectionSet sels

fragmentDefinition :: FragmentDefinition -> Builder
fragmentDefinition (FragmentDefinition name tc dirs sels) =
  stringUtf8 "fragment "
  <> nameB name
  <> stringUtf8 " on "
  <> nameB (unNamedType tc)
  <> optempty directives dirs
  <> selectionSet sels

directives :: [Directive] -> Builder
directives = mconcat . intersperse (charUtf8 ' ') . map directive

directive :: Directive -> Builder
directive (Directive name args) =
  charUtf8 '@' <> nameB name <> optempty arguments args

arguments :: [Argument] -> Builder
arguments xs = charUtf8 '(' <> args <> charUtf8 ')'
  where args = mconcat $ intersperse (charUtf8 ',') $ map argument xs

argument :: Argument -> Builder
argument (Argument name val) = nameB name <> stringUtf8 ": " <> value val

variableDefinitions :: [VariableDefinition] -> Builder
variableDefinitions vars = mconcat [ charUtf8 '('
                                   , mconcat vars'
                                   , charUtf8 ')'
                                   ]
  where vars' = intersperse (charUtf8 ',') $ map variableDefinition vars

variableDefinition :: VariableDefinition -> Builder
variableDefinition (VariableDefinition var ty defVal) =
  variable var <> stringUtf8 ": " <> type_ ty <> maybe "" defaultValue defVal

defaultValue :: DefaultValue -> Builder
defaultValue v = stringUtf8 " =" <> charUtf8 ' ' <> valueC v


-- | Type Reference

type_ :: GType -> Builder
type_ (TypeNamed n x) = nameB (unNamedType x) <> nonNull n
type_ (TypeList  n x) = listType x <> nonNull n

listType :: ListType -> Builder
listType (ListType ty) = charUtf8 '[' <> type_ ty <> charUtf8 ']'

nonNull :: Nullability -> Builder
nonNull n = bool (charUtf8 '!') mempty $ unNullability n

-- | Primitives

value :: Value -> Builder
value = \case
  VVariable v -> variable v
  VInt i      -> int32Dec i
  VFloat d    -> doubleDec d
  VString s   -> stringValue s
  VBoolean b  -> fromBool b
  VNull       -> stringUtf8 "null"
  VList xs    -> listValue xs
  VObject o   -> objectValue o
  VEnum ev    -> nameB $ unEnumValue ev

stringValue :: StringValue -> Builder
stringValue (StringValue s) =
  mconcat [ charUtf8 '"', fromText s, charUtf8 '"' ]

variable :: Variable -> Builder
variable (Variable v) = charUtf8 '$' <> nameB v

listValue :: ListValue -> Builder
listValue (ListValueG xs) = mconcat [ charUtf8 '[' , li , charUtf8 ']' ]
  where
    li = mconcat $ intersperse (charUtf8 ',') $ map value xs

objectValue :: ObjectValue -> Builder
objectValue (ObjectValueG o) = mconcat [ charUtf8 '{', vals, charUtf8 '}' ]
  where
    vals = mconcat $ intersperse (charUtf8 ',') $ map objectField o

objectField :: ObjectField -> Builder
objectField (ObjectFieldG name val) =
  mconcat [ nameB name, charUtf8 ':', value val ]

valueC :: ValueConst -> Builder
valueC = \case
  VCInt i      -> int32Dec i
  VCFloat d    -> doubleDec d
  VCString s   -> stringValue s
  VCBoolean b  -> fromBool b
  VCNull       -> stringUtf8 "null"
  VCList xs    -> listValueC xs
  VCObject o   -> objectValueC o
  VCEnum ev    -> nameB $ unEnumValue ev

listValueC :: ListValueC -> Builder
listValueC (ListValueG xs) = mconcat [ charUtf8 '[' , li , charUtf8 ']' ]
  where
    li = mconcat $ intersperse (charUtf8 ',') $ map valueC xs

objectValueC :: ObjectValueC -> Builder
objectValueC (ObjectValueG o) = mconcat [ charUtf8 '{', vals, charUtf8 '}' ]
  where
    vals = mconcat $ intersperse (charUtf8 ',') $ map objectFieldC o

objectFieldC :: ObjectFieldC -> Builder
objectFieldC (ObjectFieldG name val) =
  nameB name <> stringUtf8 ": " <> valueC val


nameB :: Name -> Builder
nameB (Name n) = fromText n

fromBool :: Bool -> Builder
fromBool True  = stringUtf8 "true"
fromBool False = stringUtf8 "false"

fromText :: Text -> Builder
fromText = LT.encodeUtf8Builder . LT.fromStrict

optempty :: (Eq a, Monoid a, Monoid b) => (a -> b) -> a -> b
optempty f xs = if xs == mempty then mempty else f xs
