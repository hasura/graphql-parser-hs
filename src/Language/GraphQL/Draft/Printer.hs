module Language.GraphQL.Draft.Printer where

import qualified Data.HashMap.Strict as M

import Data.Void
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Int (Int32)
import Data.String (IsString)
import Data.Bool (bool)
import Data.List (intersperse)
import Data.Foldable (fold)
import Data.Maybe (fromMaybe)

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

  nodeP :: Variable var => TypedOperationDefinition var -> a
  nodeP = node

  selectionSetP :: Variable var => SelectionSet var -> a
  selectionSetP = selectionSet

-- | the pretty printer implementation

executableDocument :: (Variable var, Printer a) => ExecutableDocument var -> a
executableDocument ed =
  mconcat $ intersperse (charP '\n') $ map executableDefinition $
  getExecutableDefinitions ed

executableDefinition :: (Variable var, Printer a) => ExecutableDefinition var -> a
executableDefinition = \case
  ExecutableDefinitionOperation d -> operationDefinition d
  ExecutableDefinitionFragment  d -> fragmentDefinition d

operationDefinition :: (Variable var, Printer a) => OperationDefinition var -> a
operationDefinition = \case
  OperationDefinitionUnTyped selSet -> selectionSetP selSet
  OperationDefinitionTyped op       -> typedOperationDefinition op

typedOperationDefinition :: (Variable var, Printer a) => TypedOperationDefinition var -> a
typedOperationDefinition op =
  operationType (_todType op) <> charP ' ' <> nodeP op

operationType :: Printer a => OperationType -> a
operationType = \case
  OperationTypeQuery        -> "query"
  OperationTypeMutation     -> "mutation"
  OperationTypeSubscription -> "subscription"

-- TODO: add horizontal nesting
node :: (Variable var, Printer a) => TypedOperationDefinition var -> a
node (TypedOperationDefinition _ name vars dirs sels) =
     maybe mempty nameP name
  <> optempty variableDefinitions vars
  <> optempty directives dirs
  <> charP ' '
  <> selectionSetP sels

-- TODO: add horizontal nesting
selectionSet :: (Variable var, Printer a) => SelectionSet var -> a
selectionSet [] = mempty
selectionSet xs =
  "{ " <> mconcat (intersperse (charP ' ') (map selection xs)) <> " }"

selection :: (Variable var, Printer a) => Selection var -> a
selection = \case
  SelectionField fld          -> field fld
  SelectionFragmentSpread fs  -> fragmentSpread fs
  SelectionInlineFragment ilf -> inlineFragment ilf

field :: (Variable var, Printer a) => Field var -> a
field (Field alias name args dirs selSets) =
  optAlias alias
  <> nameP name
  <> optempty arguments args
  <> optempty directives dirs
  <> charP ' '
  <> selectionSetP selSets

optAlias :: Printer a => Maybe Name -> a
optAlias = maybe mempty (\a -> nameP a <> textP ": ")

fragmentSpread :: (Variable var, Printer a) => FragmentSpread var -> a
fragmentSpread (FragmentSpread name ds) =
  "..." <> nameP name <> optempty directives ds

inlineFragment :: (Variable var, Printer a) => InlineFragment var -> a
inlineFragment (InlineFragment tc ds sels) =
  "... "
  <> maybe mempty ((textP "on" <>) . nameP) tc
  <> optempty directives ds
  <> selectionSetP sels

fragmentDefinition :: Printer a => FragmentDefinition -> a
fragmentDefinition (FragmentDefinition name tc dirs sels) =
  "fragment "
  <> nameP name
  <> " on "
  <> nameP tc
  <> optempty directives dirs
  <> selectionSetP sels

directives :: (Variable var, Printer a) => [Directive var] -> a
directives = mconcat . intersperse (charP ' ') . map directive

directive :: (Variable var, Printer a) => Directive var -> a
directive (Directive name args) =
  charP '@' <> nameP name <> optempty arguments args

arguments :: (Variable var, Printer a) => HashMap Name (Value var) -> a
arguments xs = charP '(' <> objectFields xs <> charP ')'

variableDefinitions :: Printer a => [VariableDefinition] -> a
variableDefinitions vars = mconcat [ charP '('
                                   , mconcat vars'
                                   , charP ')'
                                   ]
  where vars' = intersperse (charP ',') $ map variableDefinition vars

variableDefinition :: Printer a => VariableDefinition -> a
variableDefinition (VariableDefinition var ty defVal) =
  variable var <> ": " <> graphQLType ty <> maybe mempty defaultValue defVal

defaultValue :: Printer a => Value Void -> a
defaultValue v = " = " <> value v

-- | Type Reference

graphQLType :: Printer a => GType -> a
graphQLType (TypeNamed n x) = nameP x <> nonNull n
graphQLType (TypeList  n x) = listType x <> nonNull n

listType :: Printer a => GType -> a
listType ty = charP '[' <> graphQLType ty <> charP ']'

nonNull :: Printer a => Nullability -> a
nonNull n = bool (charP '!') mempty $ unNullability n

-- | Primitives

class Variable var where
  variable :: Printer a => var -> a
instance Variable Void where
  variable = absurd
instance Variable Name where
  variable v = charP '$' <> nameP v

value :: (Variable var, Printer a) => Value var -> a
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

stringValue :: Printer a => Text -> a
stringValue s = mconcat [ charP '"', textP s, charP '"' ]

listValue :: (Variable var, Printer a) => [Value var] -> a
listValue xs = mconcat [ charP '[' , li , charP ']' ]
  where
    li = mconcat $ intersperse (charP ',') $ map value xs

objectValue :: (Variable var, Printer a) => HashMap Name (Value var) -> a
objectValue o = charP '{' <> objectFields o <> charP '}'

objectFields :: (Variable var, Printer a) => HashMap Name (Value var) -> a
objectFields o = mconcat $ intersperse (charP ',') $ map objectField $ M.toList o
  where objectField (name, val) = nameP name <> ": " <> value val

fromBool :: Printer a => Bool -> a
fromBool True  = "true"
fromBool False = "false"

optempty :: (Foldable f, Monoid b) => (f a -> b) -> f a -> b
optempty f xs
  | null xs   = mempty
  | otherwise = f xs
