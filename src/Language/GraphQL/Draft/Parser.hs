{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Description: Parse text into GraphQL ASTs
module Language.GraphQL.Draft.Parser
  ( executableDocument
  , parseExecutableDoc
  -- * Parsers for 'AST.TypeSystemDefinition'.
  , schemaDefinition
  , typeSystemDefinition
  -- * Parsers for 'AST.SchemaDocument'.
  , schemaDocument
  , parseSchemaDoc
  -- * Small utility parsers.
  , value
  , parseValueConst
  , name
  -- * Parsers for GraphQL types.
  , graphQLType
  , parseGraphQLType

  , Parser
  , runParser
  ) where

import           Protolude                     hiding (option)

import           Control.Applicative           (many, optional, (<|>))
import           Control.Monad.Fail            (fail)
import           Data.Aeson.Parser             (jstring)
import qualified Data.Attoparsec.ByteString    as A
import           Data.Attoparsec.Text          (Parser, anyChar, char, many1,
                                                match, option, scan, scientific,
                                                sepBy1, (<?>))
import qualified Data.Attoparsec.Text          as AT
import           Data.Char                     (isAsciiLower, isAsciiUpper,
                                                isDigit)
import           Data.Scientific               (Scientific)
import           Data.Text                     (find)

import qualified Language.GraphQL.Draft.Syntax as AST

-- * Document

executableDocument :: Parser AST.ExecutableDocument
executableDocument =
  whiteSpace *>
  (AST.ExecutableDocument <$> many1 definitionExecutable)
  <?> "query document error!"

runParser :: AT.Parser a -> Text -> Either Text a
runParser parser t =
  either (Left . toS) return $ AT.parseOnly (parser <* AT.endOfInput) t

parseExecutableDoc :: Text -> Either Text AST.ExecutableDocument
parseExecutableDoc = runParser executableDocument

-- * Schema definition.

-- | Parser for a 'AST.SchemaDefinition'.
schemaDefinition :: Parser AST.SchemaDefinition
schemaDefinition = tok "schema" *> (
    AST.SchemaDefinition
      <$> optional directives
      <*> braces (many1 rootOperationTypeDefinition)
  )

-- | Parser for a 'AST.RootOperationTypeDefinition'.
rootOperationTypeDefinition :: Parser AST.RootOperationTypeDefinition
rootOperationTypeDefinition =
  AST.RootOperationTypeDefinition
    <$> (operationType <* tok ":")
    <*> (AST.NamedType <$> name)

-- | Parser for a 'AST.TypeSystemDefinition'.
typeSystemDefinition :: Parser [AST.TypeSystemDefinition]
typeSystemDefinition = concat <$> many1 (
      (\(AST.SchemaDocument d) -> AST.TypeSystemDefinitionType <$> d) <$> schemaDocument
  <|> (: []) . AST.TypeSystemDefinitionSchema <$> schemaDefinition )

-- | Parser for a schema document.
schemaDocument :: Parser AST.SchemaDocument
schemaDocument =
  whiteSpace *> (AST.SchemaDocument <$> many1 typeDefinition)
  <?> "type document error"

parseSchemaDoc :: Text -> Either Text AST.SchemaDocument
parseSchemaDoc = runParser schemaDocument

definitionExecutable :: Parser AST.ExecutableDefinition
definitionExecutable =
  AST.ExecutableDefinitionOperation <$> operationDefinition
  <|> AST.ExecutableDefinitionFragment <$> fragmentDefinition
  <?> "definition error!"

operationDefinition :: Parser AST.OperationDefinition
operationDefinition =
  AST.OperationDefinitionTyped <$> typedOperationDef
  <|> (AST.OperationDefinitionUnTyped <$> selectionSet)
  <?> "operationDefinition error!"

operationType :: Parser AST.OperationType
operationType =
  AST.OperationTypeQuery <$ tok "query"
  <|> AST.OperationTypeMutation <$ tok "mutation"
  <|> AST.OperationTypeSubscription <$ tok "subscription"

typedOperationDef :: Parser AST.TypedOperationDefinition
typedOperationDef =
  AST.TypedOperationDefinition
  <$> operationType
  <*> optional name
  <*> optempty variableDefinitions
  <*> optempty directives
  <*> selectionSet

variableDefinitions :: Parser [AST.VariableDefinition]
variableDefinitions = parens (many1 variableDefinition)

variableDefinition :: Parser AST.VariableDefinition
variableDefinition =
  AST.VariableDefinition <$> variable
                         <*  tok ":"
                         <*> graphQLType
                         <*> optional defaultValue

defaultValue :: Parser AST.DefaultValue
defaultValue = tok "=" *> valueConst

variable :: Parser AST.Variable
variable = AST.Variable <$ tok "$" <*> name

selectionSet :: Parser AST.SelectionSet
selectionSet = braces $ many1 selection

selection :: Parser AST.Selection
selection = AST.SelectionField <$> field
            -- Inline first to catch `on` case
        <|> AST.SelectionInlineFragment <$> inlineFragment
        <|> AST.SelectionFragmentSpread <$> fragmentSpread
        <?> "selection error!"

aliasAndFld :: Parser (Maybe AST.Alias, AST.Name)
aliasAndFld = do
  n <- name
  colonM <- optional (tok ":")
  case colonM of
    Just _  -> (,) (Just $ AST.Alias n) <$> name
    Nothing -> return (Nothing, n)
{-# INLINE aliasAndFld #-}

field :: Parser AST.Field
field = do
  (alM, n) <- aliasAndFld
  AST.Field alM n
   <$> optempty arguments
   <*> optempty directives
   <*> optempty selectionSet

arguments :: Parser [AST.Argument]
arguments = parens $ many1 argument

argument :: Parser AST.Argument
argument = AST.Argument <$> name <* tok ":" <*> value

-- * Fragments

fragmentSpread :: Parser AST.FragmentSpread
-- TODO: Make sure it fails when `... on`.
-- See https://facebook.github.io/graphql/#FragmentSpread
fragmentSpread = AST.FragmentSpread
  <$  tok "..."
  <*> name
  <*> optempty directives

-- InlineFragment tried first in order to guard against 'on' keyword
inlineFragment :: Parser AST.InlineFragment
inlineFragment = AST.InlineFragment
  <$  tok "..."
  <*> optional (tok "on" *> typeCondition)
  <*> optempty directives
  <*> selectionSet

fragmentDefinition :: Parser AST.FragmentDefinition
fragmentDefinition = AST.FragmentDefinition
  <$  tok "fragment"
  <*> name
  <*  tok "on"
  <*> typeCondition
  <*> optempty directives
  <*> selectionSet

typeCondition :: Parser AST.TypeCondition
typeCondition = namedType

-- * Values
parseValueConst :: Text -> Either Text AST.ValueConst
parseValueConst = runParser valueConst

valueConst :: Parser AST.ValueConst
valueConst = tok (
      (fmap (either AST.VCFloat AST.VCInt) number <?> "number")
  <|> AST.VCNull     <$  literal "null"
  <|> AST.VCBoolean  <$> (booleanValue <?> "booleanValue")
  <|> AST.VCString   <$> (stringValue <?> "stringValue")
  -- `true` and `false` have been tried before
  <|> AST.VCEnum     <$> (fmap AST.EnumValue name <?> "name")
  <|> AST.VCList     <$> (listValueC <?> "listValue")
  <|> AST.VCObject   <$> (objectValueC <?> "objectValue")
  <?> "value (const) error!"
  )

number :: Parser (Either Scientific Integer)
number = do
  (numText, num) <- match (tok scientific)
  pure $ case Data.Text.find (\c -> c == '.' || c == 'e' || c == 'E') numText of
      -- Number specified with decimals and/or scientific notation, so
      -- store as a 'Scientific'.
    Just _ -> Left num
      -- No '.' and not in scientific notation, so safe to convert to integer.
    Nothing -> Right (floor num)

-- This will try to pick the first type it can runParser. If you are working with
-- explicit types use the `typedValue` parser.
value :: Parser AST.Value
value = tok (
      AST.VVariable <$> (variable <?> "variable")
  <|> (fmap (either AST.VFloat AST.VInt) number <?> "number")
  <|> AST.VNull     <$  literal "null"
  <|> AST.VBoolean  <$> (booleanValue <?> "booleanValue")
  <|> AST.VString   <$> (stringValue <?> "stringValue")
  -- `true` and `false` have been tried before
  <|> AST.VEnum     <$> (fmap AST.EnumValue name <?> "name")
  <|> AST.VList     <$> (listValue <?> "listValue")
  <|> AST.VObject   <$> (objectValue <?> "objectValue")
  <?> "value error!"
  )

booleanValue :: Parser Bool
booleanValue
  =   True  <$ literal "true"
  <|> False <$ literal "false"

stringValue :: Parser AST.StringValue
stringValue = do
  parsed <- char '"' *> jstring_
  case unescapeText parsed of
    Left err      -> fail err
    Right escaped -> pure (AST.StringValue escaped)
  where
    -- | Parse a string without a leading quote, ignoring any escaped characters.
    jstring_ :: Parser Text
    jstring_ = scan startState go <* anyChar

    startState = False
    go a c
      | a = Just False
      | c == '"' = Nothing
      | otherwise = let a' = c == backslash
                    in Just a'
      where backslash = '\\'

    -- | Unescape a string.
    --
    -- Turns out this is really tricky, so we're going to cheat by
    -- reconstructing a literal string (by putting quotes around it) and
    -- delegating all the hard work to Aeson.
    unescapeText str = A.parseOnly jstring ("\"" <> encodeUtf8 str <> "\"")

-- Notice it can be empty
listValueG :: Parser a -> Parser (AST.ListValueG a)
listValueG val = AST.ListValueG <$> brackets (many val)

listValue :: Parser AST.ListValue
listValue = listValueG value

listValueC :: Parser AST.ListValueC
listValueC = listValueG valueConst

-- Notice it can be empty
objectValueG :: Parser a -> Parser (AST.ObjectValueG a)
objectValueG p = AST.ObjectValueG <$> braces (many (objectFieldG p <?> "objectField"))

objectValue :: Parser AST.ObjectValue
objectValue = objectValueG value

objectValueC :: Parser AST.ObjectValueC
objectValueC = objectValueG valueConst

objectFieldG :: Parser a -> Parser (AST.ObjectFieldG a)
objectFieldG p = AST.ObjectFieldG <$> name <* tok ":" <*> p

-- * Directives

directives :: Parser [AST.Directive]
directives = many1 directive

directive :: Parser AST.Directive
directive = AST.Directive
  <$  tok "@"
  <*> name
  <*> optempty arguments

-- * Type Reference

graphQLType :: Parser AST.GType
graphQLType =
    (flip AST.TypeList <$> listType <*> nullability)
    <|> (flip AST.TypeNamed <$> namedType <*> nullability)
    <?> "graphQLType error!"

parseGraphQLType :: Text -> Either Text AST.GType
parseGraphQLType = runParser graphQLType

namedType :: Parser AST.NamedType
namedType = AST.NamedType <$> name

listType :: Parser AST.ListType
listType = AST.ListType <$> brackets graphQLType

nullability :: Parser AST.Nullability
nullability =
  (tok "!" $> AST.Nullability False)
  <|> pure (AST.Nullability True)

-- * Type Definition

typeDefinition :: Parser AST.TypeDefinition
typeDefinition =
      AST.TypeDefinitionObject        <$> objectTypeDefinition
  <|> AST.TypeDefinitionInterface     <$> interfaceTypeDefinition
  <|> AST.TypeDefinitionUnion         <$> unionTypeDefinition
  <|> AST.TypeDefinitionScalar        <$> scalarTypeDefinition
  <|> AST.TypeDefinitionEnum          <$> enumTypeDefinition
  <|> AST.TypeDefinitionInputObject   <$> inputObjectTypeDefinition
  <?> "typeDefinition error!"

optDesc :: Parser (Maybe AST.Description)
optDesc = optional (AST.Description . AST.unStringValue <$> stringValue)

objectTypeDefinition :: Parser AST.ObjectTypeDefinition
objectTypeDefinition = AST.ObjectTypeDefinition
  <$> optDesc
  <*  tok "type"
  <*> name
  <*> optempty interfaces
  <*> optempty directives
  <*> fieldDefinitions

interfaces :: Parser [AST.NamedType]
interfaces = tok "implements" *> many1 namedType

fieldDefinitions :: Parser [AST.FieldDefinition]
fieldDefinitions = braces $ many1 fieldDefinition

fieldDefinition :: Parser AST.FieldDefinition
fieldDefinition = AST.FieldDefinition
  <$> optDesc
  <*> name
  <*> optempty argumentsDefinition
  <*  tok ":"
  <*> graphQLType
  <*> optempty directives

argumentsDefinition :: Parser AST.ArgumentsDefinition
argumentsDefinition = parens $ many1 inputValueDefinition

interfaceTypeDefinition :: Parser AST.InterfaceTypeDefinition
interfaceTypeDefinition = AST.InterfaceTypeDefinition
  <$> optDesc
  <*  tok "interface"
  <*> name
  <*> optempty directives
  <*> fieldDefinitions

unionTypeDefinition :: Parser AST.UnionTypeDefinition
unionTypeDefinition = AST.UnionTypeDefinition
  <$> optDesc
  <*  tok "union"
  <*> name
  <*> optempty directives
  <*  tok "="
  <*> unionMembers

unionMembers :: Parser [AST.NamedType]
unionMembers = namedType `sepBy1` tok "|"

scalarTypeDefinition :: Parser AST.ScalarTypeDefinition
scalarTypeDefinition = AST.ScalarTypeDefinition
  <$> optDesc
  <*  tok "scalar"
  <*> name
  <*> optempty directives

enumTypeDefinition :: Parser AST.EnumTypeDefinition
enumTypeDefinition = AST.EnumTypeDefinition
  <$> optDesc
  <*  tok "enum"
  <*> name
  <*> optempty directives
  <*> enumValueDefinitions

enumValueDefinitions :: Parser [AST.EnumValueDefinition]
enumValueDefinitions = braces $ many1 enumValueDefinition

enumValueDefinition :: Parser AST.EnumValueDefinition
enumValueDefinition = AST.EnumValueDefinition
  <$> optDesc
  <*> enumValue
  <*> optempty directives

-- TODO: should not be one of true/false/null
enumValue :: Parser AST.EnumValue
enumValue = AST.EnumValue <$> name

inputObjectTypeDefinition :: Parser AST.InputObjectTypeDefinition
inputObjectTypeDefinition = AST.InputObjectTypeDefinition
  <$> optDesc
  <*  tok "input"
  <*> name
  <*> optempty directives
  <*> inputValueDefinitions

inputValueDefinitions :: Parser [AST.InputValueDefinition]
inputValueDefinitions = braces $ many1 inputValueDefinition

inputValueDefinition :: Parser AST.InputValueDefinition
inputValueDefinition = AST.InputValueDefinition
  <$> optDesc
  <*> name
  <*  tok ":"
  <*> graphQLType
  <*> optional defaultValue

-- * Internal

tok :: AT.Parser a -> AT.Parser a
tok p = p <* whiteSpace
{-# INLINE tok #-}

-- |
-- Literal functions in the same fashion as `tok`,
-- however there are issues using `tok` when the token may be followed by additional /a-z0-9/i characters.
-- This manifests in bugs such as #20 where columns in on_conflict clauses prefixed with keywords
-- e.g. "nullColumn" actually end up parsing as "[null, Column]".
--
-- Adding in a seperate lexing pass would probably be the right way to resolve this behaviour.
-- This is a simple initial fix to address the bug with more involved changes being able to be
-- considered seperately.
literal :: AT.Parser a -> AT.Parser a
literal p = p <* ends <* whiteSpace
{-# INLINE literal #-}

ends :: AT.Parser ()
ends = do
  mc <- AT.peekChar
  case mc of
    Nothing -> pure ()
    Just c  ->
      if isNonFirstChar c
         then mzero
         else pure ()

comment :: Parser ()
comment =
  AT.char '#' *>
  AT.skipWhile (\c -> c /= '\n' && c /= '\r' )
{-# INLINE comment #-}

isSpaceLike :: Char -> Bool
isSpaceLike c =
  c == '\t' || c == ' ' || c == '\n' || c == '\r' || c == ','
{-# INLINE isSpaceLike #-}

whiteSpace :: AT.Parser ()
whiteSpace = do
  AT.skipWhile isSpaceLike
  (comment *> whiteSpace) <|> pure ()

-- whiteSpace :: AT.Parser ()
-- whiteSpace =
--   void $ AT.scan False $ \st c ->
--   if | not st && isSpaceLike c        -> Just False
--      | not st && c == '#'             -> Just True
--      | not st                         -> Nothing
--      | st && (c == '\r' || c == '\n') -> Just False
--      | st                             -> Just True
-- {-# INLINE whiteSpace #-}

name :: AT.Parser AST.Name
name =
  AST.Name <$> tok ((<>) <$> AT.takeWhile1 isFirstChar
                         <*> AT.takeWhile isNonFirstChar)
{-# INLINE name #-}

isFirstChar :: Char -> Bool
isFirstChar x = isAsciiLower x || isAsciiUpper x || x == '_'
{-# INLINE isFirstChar #-}

isNonFirstChar :: Char -> Bool
isNonFirstChar x = isFirstChar x || isDigit x
{-# INLINE isNonFirstChar #-}

parens :: Parser a -> Parser a
parens = between "(" ")"

braces :: Parser a -> Parser a
braces = between "{" "}"

brackets :: Parser a -> Parser a
brackets = between "[" "]"

between :: Parser Text -> Parser Text -> Parser a -> Parser a
between open close p = tok open *> p <* tok close

-- `empty` /= `pure mempty` for `Parser`.
optempty :: Monoid a => Parser a -> Parser a
optempty = option mempty
