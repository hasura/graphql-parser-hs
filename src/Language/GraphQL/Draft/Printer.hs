module Language.GraphQL.Draft.Printer where

import qualified Data.ByteString.Builder       as BS
import qualified Data.HashMap.Strict.InsOrd    as OMap
import qualified Data.Text.Lazy                as LT hiding (singleton)
import qualified Data.Text.Lazy.Builder        as LT
import qualified Data.Text.Lazy.Encoding       as LT
import qualified Data.Text.Prettyprint.Doc     as PP
import qualified Text.Builder                  as Text

import           Data.Bool                     (bool)
import           Data.HashMap.Strict.InsOrd    (InsOrdHashMap)
import           Data.List                     (intersperse)
import           Data.Scientific
import           Data.String                   (IsString)
import           Data.Text                     (Text, pack)
import           Data.Void

import           Language.GraphQL.Draft.Syntax

class (Monoid a, IsString a) => Printer a where
  stringP  :: String -> a
  textP    :: Text -> a
  charP    :: Char -> a
  intP     :: Integer -> a
  doubleP  :: Scientific -> a

  {-# MINIMAL stringP, textP, charP, intP, doubleP #-}

  nameP    :: Name -> a
  nameP    = textP . unName

  nodeP :: (Print (frag var), Print var) => TypedOperationDefinition frag var -> a
  nodeP = node

  selectionSetP :: (Print (frag var), Print var) => SelectionSet frag var -> a
  selectionSetP = selectionSet

instance Printer BS.Builder where
  stringP = BS.stringUtf8
  {-# INLINE stringP #-}

  textP = LT.encodeUtf8Builder . LT.fromStrict
  {-# INLINE textP #-}

  charP = BS.charUtf8
  {-# INLINE charP #-}

  intP = BS.integerDec
  {-# INLINE intP #-}

  doubleP = BS.stringUtf8 . show
  {-# INLINE doubleP #-}

instance Printer LT.Builder where
  stringP = LT.fromString
  {-# INLINE stringP #-}

  textP   = LT.fromText
  {-# INLINE textP #-}

  charP   = LT.singleton
  {-# INLINE charP #-}

  intP    = LT.fromString . show
  {-# INLINE intP #-}

  doubleP = LT.fromString . show
  {-# INLINE doubleP #-}

instance Printer (PP.Doc Text) where
  stringP       = PP.pretty
  {-# INLINE stringP #-}

  textP         = PP.pretty
  {-# INLINE textP #-}

  charP         = PP.pretty
  {-# INLINE charP #-}

  intP          = PP.pretty
  {-# INLINE intP #-}

  doubleP sc    = PP.pretty $ pack $ show sc
  {-# INLINE doubleP #-}

  nameP         = PP.pretty
  {-# INLINE nameP #-}

instance Printer Text.Builder where
  stringP = Text.string
  {-# INLINE stringP #-}

  textP   = Text.text
  {-# INLINE textP #-}

  charP   = Text.char
  {-# INLINE charP #-}

  intP    = Text.decimal
  {-# INLINE intP #-}

  doubleP = Text.string . show
  {-# INLINE doubleP #-}

class Print a where
  printP :: Printer b => a -> b
instance Print Void where
  printP = absurd
instance Print Name where
  printP = nameP

renderExecutableDoc :: ExecutableDocument Name -> Text
renderExecutableDoc = Text.run . executableDocument

-- | the pretty printer implementation

executableDocument :: (Print var, Printer a) => ExecutableDocument var -> a
executableDocument ed =
  mconcat $ intersperse (charP '\n') $ map executableDefinition $
  getExecutableDefinitions ed

executableDefinition :: (Print var, Printer a) => ExecutableDefinition var -> a
executableDefinition = \case
  ExecutableDefinitionOperation d -> operationDefinition d
  ExecutableDefinitionFragment  d -> fragmentDefinition d

operationDefinition :: (Print (frag var), Print var, Printer a) => OperationDefinition frag var -> a
operationDefinition = \case
  OperationDefinitionUnTyped selSet -> selectionSetP selSet
  OperationDefinitionTyped op       -> typedOperationDefinition op

typedOperationDefinition :: (Print (frag var), Print var, Printer a) => TypedOperationDefinition frag var -> a
typedOperationDefinition op =
  operationType (_todType op) <> charP ' ' <> nodeP op

operationType :: Printer a => OperationType -> a
operationType = \case
  OperationTypeQuery        -> "query"
  OperationTypeMutation     -> "mutation"
  OperationTypeSubscription -> "subscription"

-- TODO: add horizontal nesting
node :: (Print (frag var), Print var, Printer a) => TypedOperationDefinition frag var -> a
node (TypedOperationDefinition _ name vars dirs sels) =
     maybe mempty nameP name
  <> optempty variableDefinitions vars
  <> optempty directives dirs
  <> charP ' '
  <> selectionSetP sels

-- TODO: add horizontal nesting
selectionSet :: (Print (frag var), Print var, Printer a) => SelectionSet frag var -> a
selectionSet [] = mempty
selectionSet xs =
  "{ " <> mconcat (intersperse (charP ' ') (map selection xs)) <> " }"

selection :: (Print (frag var), Print var, Printer a) => Selection frag var -> a
selection = \case
  SelectionField fld          -> field fld
  SelectionFragmentSpread fs  -> printP fs
  SelectionInlineFragment ilf -> inlineFragment ilf

field :: (Print (frag var), Print var, Printer a) => Field frag var -> a
field (Field alias name args dirs selSets) =
  optAlias alias
  <> nameP name
  <> optempty arguments args
  <> optempty directives dirs
  <> charP ' '
  <> selectionSetP selSets

optAlias :: Printer a => Maybe Name -> a
optAlias = maybe mempty (\a -> nameP a <> textP ": ")

inlineFragment :: (Print (frag var), Print var, Printer a) => InlineFragment frag var -> a
inlineFragment (InlineFragment tc ds sels) =
  "... "
  <> maybe mempty ((textP "on " <>) . nameP) tc
  <> optempty directives ds
  <> selectionSetP sels

instance Print var => Print (FragmentSpread var) where
  printP (FragmentSpread name ds) =
    "..." <> nameP name <> optempty directives ds

instance Print (NoFragments var) where
  printP = \case{}

fragmentDefinition :: Printer a => FragmentDefinition -> a
fragmentDefinition (FragmentDefinition name tc dirs sels) =
  "fragment "
  <> nameP name
  <> " on "
  <> nameP tc
  <> optempty directives dirs
  <> selectionSetP sels

directives :: (Print var, Printer a) => [Directive var] -> a
directives = mconcat . intersperse (charP ' ') . map directive

directive :: (Print var, Printer a) => Directive var -> a
directive (Directive name args) =
  charP '@' <> nameP name <> optempty arguments args

arguments :: (Print var, Printer a) => InsOrdHashMap Name (Value var) -> a
arguments xs = charP '(' <> objectFields xs <> charP ')'

variableDefinitions :: Printer a => [VariableDefinition] -> a
variableDefinitions vars = mconcat [ charP '('
                                   , mconcat vars'
                                   , charP ')'
                                   ]
  where vars' = intersperse (charP ',') $ map variableDefinition vars

variableDefinition :: Printer a => VariableDefinition -> a
variableDefinition (VariableDefinition var ty defVal) =
  variableP var <> ": " <> graphQLType ty <> maybe mempty defaultValue defVal

defaultValue :: Printer a => Value Void -> a
defaultValue v = " = " <> value v

description :: Printer a => Maybe Description -> a
description Nothing     = mempty
description (Just desc) = (stringValue $ unDescription desc) <> " \n"
-- | Type Reference

graphQLType :: Printer a => GType -> a
graphQLType (TypeNamed n x) = nameP x <> nonNull n
graphQLType (TypeList  n x) = listType x <> nonNull n

listType :: Printer a => GType -> a
listType ty = charP '[' <> graphQLType ty <> charP ']'

nonNull :: Printer a => Nullability -> a
nonNull n = bool (charP '!') mempty $ unNullability n

-- | Primitives

variableP :: (Print a, Printer b) => a -> b
variableP v = charP '$' <> printP v

value :: (Print var, Printer a) => Value var -> a
value = \case
  VVariable v -> variableP v
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

listValue :: (Print var, Printer a) => [Value var] -> a
listValue xs = mconcat [ charP '[' , li , charP ']' ]
  where
    li = mconcat $ intersperse (charP ',') $ map value xs

objectValue :: (Print var, Printer a) => InsOrdHashMap Name (Value var) -> a
objectValue o = charP '{' <> objectFields o <> charP '}'

objectFields :: (Print var, Printer a) => InsOrdHashMap Name (Value var) -> a
objectFields o = mconcat $ intersperse (charP ',') $ map objectField $ OMap.toList o
  where objectField (name, val) = nameP name <> ": " <> value val

fromBool :: Printer a => Bool -> a
fromBool True  = "true"
fromBool False = "false"

optempty :: (Foldable f, Monoid b) => (f a -> b) -> f a -> b
optempty f xs
  | null xs   = mempty
  | otherwise = f xs

schemaDefinition
  :: forall a
  .  Printer a
  => SchemaDefinition
  -> a
schemaDefinition (SchemaDefinition dirs rootOpDefs) =
  "schema "
  <> maybe mempty (optempty directives) dirs
  <> " { "
  <> mconcat (intersperse (charP ' ') (map rootOperationTypeDefinition rootOpDefs))
  <> " }"

rootOperationTypeDefinition :: Printer a => RootOperationTypeDefinition -> a
rootOperationTypeDefinition (RootOperationTypeDefinition opType rootName) =
  operationType opType <> ": " <> nameP rootName

typeSystemDefinition :: Printer a => TypeSystemDefinition -> a
typeSystemDefinition (TypeSystemDefinitionSchema schemaDefn) = schemaDefinition schemaDefn
typeSystemDefinition (TypeSystemDefinitionType typeDefn)     = typeDefinitionP typeDefn

schemaDocument :: Printer a => SchemaDocument -> a
schemaDocument (SchemaDocument typeDefns) =
  mconcat $ intersperse (charP '\n') $ map typeSystemDefinition typeDefns

typeDefinitionP :: Printer a => (TypeDefinition () InputValueDefinition) -> a
typeDefinitionP (TypeDefinitionScalar scalarDefn)       = scalarTypeDefinition scalarDefn
typeDefinitionP (TypeDefinitionObject objDefn)          = objectTypeDefinition objDefn
typeDefinitionP (TypeDefinitionInterface interfaceDefn) = interfaceTypeDefinition interfaceDefn
typeDefinitionP (TypeDefinitionUnion unionDefn)         = unionTypeDefinition unionDefn
typeDefinitionP (TypeDefinitionEnum enumDefn)           = enumTypeDefinition enumDefn
typeDefinitionP (TypeDefinitionInputObject inpObjDefn)  = inputObjectTypeDefinition inpObjDefn

scalarTypeDefinition :: Printer a => ScalarTypeDefinition -> a
scalarTypeDefinition (ScalarTypeDefinition desc name dirs) =
  description desc
  <> "scalar "
  <> nameP name
  <> charP ' '
  <> optempty directives dirs

inputValueDefinition :: Printer a => InputValueDefinition -> a
inputValueDefinition (InputValueDefinition desc name gType defVal dirs) =
  description desc
  <> nameP name
  <> textP ": "
  <> graphQLType gType
  <> (maybe mempty defaultValue defVal)
  <> charP ' '
  <> optempty directives dirs

fieldDefinition :: Printer a => FieldDefinition InputValueDefinition -> a
fieldDefinition (FieldDefinition desc name args gType dirs) =
  description desc
  <> nameP name
  <>
  case args of
    [] -> mempty
    _  ->
      charP '('
      <> (mconcat $ intersperse (textP ", ") $ map inputValueDefinition args)
      <> charP ')'
  <> textP ": "
  <> graphQLType gType
  <> optempty directives dirs

objectTypeDefinition :: Printer a => ObjectTypeDefinition InputValueDefinition -> a
objectTypeDefinition (ObjectTypeDefinition desc name ifaces dirs fieldDefinitions) =
  description desc
  <> "type "
  <> nameP name
  <> optempty directives dirs
  <>
  case ifaces of
    [] -> mempty
    _  -> " implements " <> (mconcat (intersperse (textP " & ") $ map nameP ifaces))
  <> " { "
  <> (mconcat $ intersperse (charP ' ') $ map fieldDefinition fieldDefinitions)
  <> " }"

interfaceTypeDefinition :: Printer a => InterfaceTypeDefinition () InputValueDefinition -> a
interfaceTypeDefinition (InterfaceTypeDefinition desc name dirs fieldDefinitions _possibleTypes) =
  -- `possibleTypes` are not included with an interface definition in a GraphQL IDL
  description desc
  <> "interface "
  <> nameP name
  <> charP ' '
  <> optempty directives dirs
  <> " { "
  <> (mconcat $ intersperse (charP ' ') $ map fieldDefinition fieldDefinitions)
  <> " }"

unionTypeDefinition :: Printer a => UnionTypeDefinition -> a
unionTypeDefinition (UnionTypeDefinition desc name dirs members) =
  description desc
  <> "union "
  <> nameP name
  <> charP ' '
  <> optempty directives dirs
  <> textP " = "
  <> (mconcat $ intersperse (textP " | ") $ map nameP members)

enumValueDefinition :: Printer a => EnumValueDefinition -> a
enumValueDefinition (EnumValueDefinition desc name dirs) =
  description desc
  <> (nameP $ unEnumValue name)
  <> charP ' '
  <> optempty directives dirs

enumTypeDefinition :: Printer a => EnumTypeDefinition -> a
enumTypeDefinition (EnumTypeDefinition desc name dirs enumValDefns) =
  description desc
  <> "enum "
  <> nameP name
  <> optempty directives dirs
  <> " {"
  <> (mconcat $ intersperse (charP ' ') $ map enumValueDefinition enumValDefns)
  <> " }"

inputObjectTypeDefinition :: Printer a => InputObjectTypeDefinition InputValueDefinition -> a
inputObjectTypeDefinition (InputObjectTypeDefinition desc name dirs valDefns) =
  description desc
  <> "input "
  <> nameP name
  <> optempty directives dirs
  <> " {"
  <> (mconcat $ intersperse (charP ' ') $ map inputValueDefinition valDefns)
  <> " }"
