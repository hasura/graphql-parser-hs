{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module defines a printer for the @GraphQL@ language.

module Language.GraphQL.Draft.Printer.Pretty where

import           Data.Text                             (Text)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Protolude

import qualified Data.Text                             as T

import           Language.GraphQL.Draft.Syntax


renderPretty :: Doc Text -> Text
renderPretty = renderStrict . layoutPretty defaultLayoutOptions

renderCompact :: Doc Text -> Text
renderCompact = renderStrict . layoutCompact

instance Pretty Name where
  pretty = pretty. unName

executableDocument :: ExecutableDocument -> Doc Text
executableDocument doc = hsep $ punctuate "\n" $ map executableDefinition
                         $ getExecutableDefinitions doc

executableDefinition :: ExecutableDefinition -> Doc Text
executableDefinition = \case
  ExecutableDefinitionOperation d -> operationDefinition d
  ExecutableDefinitionFragment  d -> fragmentDefinition d

operationDefinition :: OperationDefinition -> Doc Text
operationDefinition = \case
  OperationDefinitionUnTyped selSet -> selectionSet selSet
  OperationDefinitionTyped op       -> typedOperationDefinition op

typedOperationDefinition :: TypedOperationDefinition -> Doc Text
typedOperationDefinition op = operationType (_todType op) <+> node op

operationType :: OperationType -> Doc Text
operationType = \case
  OperationTypeQuery        -> "query"
  OperationTypeMutation     -> "mutation"
  OperationTypeSubscription -> "subscription"

node :: TypedOperationDefinition -> Doc Text
node (TypedOperationDefinition _ name vars dirs sels) =
  pretty (fromMaybe "" name)
  <> optempty variableDefinitions vars
  <> optempty directives dirs
  <+> selectionSet sels

selectionSet :: SelectionSet -> Doc Text
selectionSet []     = ""
selectionSet selSet = nest 2 $ vbraces $ vsep (map selection selSet)

selection :: Selection -> Doc Text
selection = \case
  SelectionField fld -> field fld
  SelectionFragmentSpread fs -> fragmentSpread fs
  SelectionInlineFragment ilf -> inlineFragment ilf

field :: Field -> Doc Text
field (Field alias name args dirs selSets) =
  optAlias alias
  <+> pretty name
  <> optempty arguments args
  <> optempty directives dirs
  <+> selectionSet selSets

optAlias :: Maybe Alias -> Doc Text
optAlias = \case
  Nothing -> ""
  Just (Alias a) -> pretty a <> ":"

arguments :: [Argument] -> Doc Text
arguments xs = surround (hsep args) "(" ")"
  where args = punctuate "," $ map argument xs

argument :: Argument -> Doc Text
argument (Argument name val) = pretty name <> ":" <+> value val

value :: Value -> Doc Text
value = \case
  VVariable v -> variable v
  VInt i      -> pretty $ T.pack $ show i
  VFloat f    -> pretty $ T.pack $ show f
  VString s   -> stringValue s
  VBoolean b  -> booleanValue b
  VNull       -> pretty ("null" :: Text)
  VEnum e     -> pretty $ unEnumValue e
  VList l     -> listValue l
  VObject o   -> objectValue o

valueC :: ValueConst -> Doc Text
valueC = \case
  VCInt i      -> pretty $ T.pack $ show i
  VCFloat f    -> pretty $ T.pack $ show f
  VCString s   -> stringValue s
  VCBoolean b  -> booleanValue b
  VCNull       -> pretty ("null" :: Text)
  VCEnum e     -> pretty $ unEnumValue e
  VCList l     -> listValueC l
  VCObject o   -> objectValueC o

variable :: Variable -> Doc a
variable (Variable v) = "$" <> pretty v

variableDefinitions :: [VariableDefinition] -> Doc Text
variableDefinitions vars = surround (hsep vars') "(" ")"
  where vars' = punctuate "," $ map variableDefinition vars

variableDefinition :: VariableDefinition -> Doc Text
variableDefinition (VariableDefinition var ty defVal) =
  variable var <> ":" <+> type_ ty <> maybe "" defaultValue defVal

defaultValue :: DefaultValue -> Doc Text
defaultValue v = " =" <+> valueC v

stringValue :: StringValue -> Doc Text
stringValue (StringValue s) = surround (pretty s) "\"" "\""

booleanValue :: Bool -> Doc Text
booleanValue True  = "true"
booleanValue False = "false"

listValue :: ListValue -> Doc Text
listValue (ListValueG xs) = surround (hsep csv) "[" "]"
  where csv = punctuate "," $ map value xs

listValueC :: ListValueC -> Doc Text
listValueC (ListValueG xs) = surround (hsep csv) "[" "]"
  where csv = punctuate "," $ map valueC xs

objectValue :: ObjectValue -> Doc Text
objectValue (ObjectValueG xs) = surround (hsep vals) "{" "}"
  where vals = punctuate "," $ map objectField xs

objectValueC :: ObjectValueC -> Doc Text
objectValueC (ObjectValueG xs) = surround (hsep vals) "{" "}"
  where vals = punctuate "," $ map objectFieldC xs

objectField :: ObjectField -> Doc Text
objectField (ObjectFieldG name val) = pretty name <> ":" <+> value val

objectFieldC :: ObjectFieldC -> Doc Text
objectFieldC (ObjectFieldG name val) = pretty name <> ":" <+> valueC val

fragmentSpread :: FragmentSpread -> Doc Text
fragmentSpread (FragmentSpread name ds) =
  "..." <> pretty name <> optempty directives ds

inlineFragment :: InlineFragment -> Doc Text
inlineFragment (InlineFragment tc ds sels) =
  "... "
  <> bool "" "on" (isJust tc)
  <> pretty (fold $ fmap (unName . unNamedType) tc)
  <> optempty directives ds
  <> selectionSet sels

fragmentDefinition :: FragmentDefinition -> Doc Text
fragmentDefinition (FragmentDefinition name tc dirs sels) =
  "fragment"
  <+> pretty name <+> "on" <+> pretty (unName $ unNamedType tc)
  <> optempty directives dirs
  <> selectionSet sels

-- * Type Reference

type_ :: GType -> Doc Text
type_ (TypeNamed n x) = pretty (unNamedType x) <> nonNull n
type_ (TypeList  n x) = listType x <> nonNull n

listType :: ListType -> Doc Text
listType (ListType ty) = brackets (type_ ty)

nonNull :: Nullability -> Doc Text
nonNull n = bool "!" "" $ unNullability n


directives :: [Directive] -> Doc Text
directives = hsep . punctuate " " . map directive

directive :: Directive -> Doc Text
directive (Directive name args) =
  "@" <> pretty name <> optempty arguments args

directiveLocation :: DirectiveLocation -> Text
directiveLocation = \case
  DLExecutable dir -> executableDirectiveLocation dir
  DLTypeSystem dir -> typeSystemDirectiveLocation dir

executableDirectiveLocation :: ExecutableDirectiveLocation -> Text
executableDirectiveLocation = \case
  EDLQUERY               -> "QUERY"
  EDLMUTATION            -> "MUTATION"
  EDLSUBSCRIPTION        -> "SUBSCRIPTION"
  EDLFIELD               -> "FIELD"
  EDLFRAGMENT_DEFINITION -> "FRAGMENT_DEFINITION"
  EDLFRAGMENT_SPREAD     -> "FRAGMENT_SPREAD"
  EDLINLINE_FRAGMENT     -> "INLINE_FRAGMENT"

typeSystemDirectiveLocation :: TypeSystemDirectiveLocation -> Text
typeSystemDirectiveLocation = \case
  TSDLSCHEMA                 -> "SCHEMA"
  TSDLSCALAR                 -> "SCALAR"
  TSDLOBJECT                 -> "OBJECT"
  TSDLFIELD_DEFINITION       -> "FIELD_DEFINITION"
  TSDLARGUMENT_DEFINITION    -> "ARGUMENT_DEFINITION"
  TSDLINTERFACE              -> "INTERFACE"
  TSDLUNION                  -> "UNION"
  TSDLENUM                   -> "ENUM"
  TSDLENUM_VALUE             -> "ENUM_VALUE"
  TSDLINPUT_OBJECT           -> "INPUT_OBJECT"
  TSDLINPUT_FIELD_DEFINITION -> "INPUT_FIELD_DEFINITION"


vbraces :: Doc Text -> Doc Text
vbraces matter = vsep ["{", matter, "}"]

optempty :: (Eq a, Monoid a, Monoid b) => (a -> b) -> a -> b
optempty f xs = if xs == mempty then mempty else f xs
