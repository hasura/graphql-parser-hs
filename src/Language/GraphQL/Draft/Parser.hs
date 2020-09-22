{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Description: Parse text into GraphQL ASTs
module Language.GraphQL.Draft.Parser
  ( executableDocument
  , parseExecutableDoc
  , schemaDocument
  , parseTypeSystemDefinitions
  , parseSchemaDocument

  , Variable(..)
  , value
  , PossibleTypes(..)
  , nameParser

  , graphQLType
  , parseGraphQLType

  , Parser
  , runParser
  ) where

import qualified Data.Attoparsec.ByteString    as A
import qualified Data.Attoparsec.Text          as AT
import qualified Data.HashMap.Strict           as M
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T

import           Control.Applicative
import           Control.Monad
import           Data.Aeson.Parser             (jstring)
import           Data.Attoparsec.Text          (Parser, anyChar, char, many1,
                                                match, option, scan, scientific,
                                                sepBy1, (<?>))
import           Data.Char                     (isAsciiLower, isAsciiUpper,
                                                isDigit)
import           Data.Functor
import           Data.HashMap.Strict           (HashMap)
import           Data.Scientific               (Scientific)
import           Data.Text                     (Text, find)
import           Data.Void                     (Void)

import qualified Language.GraphQL.Draft.Syntax as AST

-- * Document

executableDocument :: Parser (AST.ExecutableDocument AST.Name)
executableDocument = whiteSpace *> (AST.ExecutableDocument <$> many1 definitionExecutable)

runParser :: AT.Parser a -> Text -> Either Text a
runParser parser t =
  either (Left . T.pack) return $ AT.parseOnly (parser <* AT.endOfInput) t

parseExecutableDoc :: Text -> Either Text (AST.ExecutableDocument AST.Name)
parseExecutableDoc = runParser executableDocument

-- | Parser for a schema document.
schemaDocument :: Parser AST.SchemaDocument
schemaDocument = whiteSpace *> (AST.SchemaDocument <$> many1 typeSystemDefinition)

parseSchemaDocument :: Text -> Either Text AST.SchemaDocument
parseSchemaDocument = runParser schemaDocument

definitionExecutable :: Parser (AST.ExecutableDefinition AST.Name)
definitionExecutable =
  AST.ExecutableDefinitionOperation <$> operationDefinition
  <|> AST.ExecutableDefinitionFragment <$> fragmentDefinition

operationDefinition :: Parser (AST.OperationDefinition AST.FragmentSpread AST.Name)
operationDefinition =
  AST.OperationDefinitionTyped <$> typedOperationDef
  <|> (AST.OperationDefinitionUnTyped <$> selectionSet)

operationTypeParser :: Parser AST.OperationType
operationTypeParser =
  AST.OperationTypeQuery <$ tok "query"
  <|> AST.OperationTypeMutation <$ tok "mutation"
  <|> AST.OperationTypeSubscription <$ tok "subscription"

typedOperationDef :: Parser (AST.TypedOperationDefinition AST.FragmentSpread AST.Name)
typedOperationDef =
  AST.TypedOperationDefinition
  <$> operationTypeParser
  <*> optional nameParser
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

defaultValue :: Parser (AST.Value Void)
defaultValue = tok "=" *> value

class Variable var where
  variable :: Parser var
instance Variable Void where
  variable = empty
instance Variable AST.Name where
  variable = tok "$" *> nameParser <?> "variable"

class PossibleTypes pos where
  possibleTypes :: Parser pos
instance PossibleTypes () where
  possibleTypes = pure ()

selectionSet :: Variable var => Parser (AST.SelectionSet AST.FragmentSpread var)
selectionSet = braces $ many1 selection

selection :: Variable var => Parser (AST.Selection AST.FragmentSpread var)
selection = AST.SelectionField <$> field
            -- Inline first to catch `on` case
        <|> AST.SelectionInlineFragment <$> inlineFragment
        <|> AST.SelectionFragmentSpread <$> fragmentSpread

aliasAndFld :: Parser (Maybe AST.Name, AST.Name)
aliasAndFld = do
  n <- nameParser
  colonM <- optional (tok ":")
  case colonM of
    Just _  -> (Just n,) <$> nameParser
    Nothing -> return (Nothing, n)
{-# INLINE aliasAndFld #-}

field :: Variable var => Parser (AST.Field AST.FragmentSpread var)
field = do
  (alM, n) <- aliasAndFld
  AST.Field alM n
   <$> optempty arguments
   <*> optempty directives
   <*> optempty selectionSet

-- * Fragments

fragmentSpread :: Variable var => Parser (AST.FragmentSpread var)
-- TODO: Make sure it fails when `... on`.
-- See https://facebook.github.io/graphql/#FragmentSpread
fragmentSpread = AST.FragmentSpread
  <$  tok "..."
  <*> nameParser
  <*> optempty directives

-- InlineFragment tried first in order to guard against 'on' keyword
inlineFragment :: Variable var => Parser (AST.InlineFragment AST.FragmentSpread var)
inlineFragment = AST.InlineFragment
  <$  tok "..."
  <*> optional (tok "on" *> nameParser)
  <*> optempty directives
  <*> selectionSet

fragmentDefinition :: Parser AST.FragmentDefinition
fragmentDefinition = AST.FragmentDefinition
  <$  tok "fragment"
  <*> nameParser
  <*  tok "on"
  <*> nameParser
  <*> optempty directives
  <*> selectionSet

-- * Values
number :: Parser (Either Scientific Integer)
number = do
  (numText, num) <- match (tok scientific)
  pure $ case Data.Text.find (\c -> c == '.' || c == 'e' || c == 'E') numText of
      -- Number specified with decimals and/or scientific notation, so
      -- store as a 'Scientific'.
    Just _  -> Left num
      -- No '.' and not in scientific notation, so safe to convert to integer.
    Nothing -> Right (floor num)

-- This will try to pick the first type it can runParser. If you are working with
-- explicit types use the `typedValue` parser.
value :: Variable var => Parser (AST.Value var)
value = tok (
      AST.VVariable <$> variable
  <|> (fmap (either AST.VFloat AST.VInt) number <?> "number")
  <|> AST.VNull     <$  literal "null"
  <|> AST.VBoolean  <$> booleanLiteral
  <|> AST.VString   <$> stringLiteral
  -- `true` and `false` have been tried before
  <|> AST.VEnum     <$> (fmap AST.EnumValue nameParser <?> "name")
  <|> AST.VList     <$> listLiteral
  <|> AST.VObject   <$> objectLiteral
  <?> "value")

booleanLiteral :: Parser Bool
booleanLiteral
  =   True  <$ literal "true"
  <|> False <$ literal "false"
  <?> "boolean"

stringLiteral :: Parser Text
stringLiteral = unescapeText =<< (char '"' *> jstring_ <?> "string")
  where
    -- | Parse a string without a leading quote, ignoring any escaped characters.
    jstring_ :: Parser Text
    jstring_ = scan False go <* anyChar

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
    unescapeText str = either fail pure $ A.parseOnly jstring ("\"" <> T.encodeUtf8 str <> "\"")

listLiteral :: Variable var => Parser [AST.Value var]
listLiteral = brackets (many value) <?> "list"

objectLiteral :: Variable var => Parser (HashMap AST.Name (AST.Value var))
objectLiteral = braces (objectFields many) <?> "object"

arguments :: Variable var => Parser (HashMap AST.Name (AST.Value var))
arguments = parens (objectFields many1) <?> "arguments"

objectFields
  :: Variable var
  => (forall b. Parser b -> Parser [b])
  -> Parser (HashMap AST.Name (AST.Value var))
objectFields several = foldM insertField M.empty =<< several objectField
  where
    objectField = (,) <$> nameParser <* tok ":" <*> value
    insertField obj (k, v)
      | k `M.member` obj = fail $ "multiple “" <> T.unpack (AST.unName k) <> "” fields"
      | otherwise        = pure (M.insert k v obj)

-- * Directives

directives :: Variable var => Parser [AST.Directive var]
directives = many1 directive

directive :: Variable var => Parser (AST.Directive var)
directive = AST.Directive
  <$  tok "@"
  <*> nameParser
  <*> optempty arguments

-- * Type Reference

graphQLType :: Parser AST.GType
graphQLType =
        (flip AST.TypeList <$> brackets graphQLType <*> nullability)
    <|> (flip AST.TypeNamed <$> nameParser <*> nullability)
    <?> "type"

parseGraphQLType :: Text -> Either Text AST.GType
parseGraphQLType = runParser graphQLType

nullability :: Parser AST.Nullability
nullability =
      (tok "!" $> AST.Nullability False)
  <|> pure (AST.Nullability True)

-- * Type Definition

rootOperationTypeDefinition :: Parser AST.RootOperationTypeDefinition
rootOperationTypeDefinition =
  AST.RootOperationTypeDefinition <$> operationTypeParser <* tok ":" <*> nameParser

schemaDefinition :: Parser AST.SchemaDefinition
schemaDefinition = AST.SchemaDefinition
  <$ tok "schema"
  <*> optional directives
  <*> rootOperationTypeDefinitions

rootOperationTypeDefinitions :: Parser [AST.RootOperationTypeDefinition]
rootOperationTypeDefinitions = braces $ many1 rootOperationTypeDefinition

typeSystemDefinition :: Parser AST.TypeSystemDefinition
typeSystemDefinition =
  AST.TypeSystemDefinitionSchema <$> schemaDefinition
  <|> AST.TypeSystemDefinitionType <$> typeDefinition

parseTypeSystemDefinitions :: Text -> Either Text [AST.TypeSystemDefinition]
parseTypeSystemDefinitions = runParser $ many1 typeSystemDefinition

typeDefinition :: Parser (AST.TypeDefinition ())
typeDefinition =
      AST.TypeDefinitionObject        <$> objectTypeDefinition
  <|> AST.TypeDefinitionInterface     <$> interfaceTypeDefinition
  <|> AST.TypeDefinitionUnion         <$> unionTypeDefinition
  <|> AST.TypeDefinitionScalar        <$> scalarTypeDefinition
  <|> AST.TypeDefinitionEnum          <$> enumTypeDefinition
  <|> AST.TypeDefinitionInputObject   <$> inputObjectTypeDefinition
  <?> "type definition"

optDesc :: Parser (Maybe AST.Description)
optDesc = optional (AST.Description <$> stringLiteral)

objectTypeDefinition :: Parser AST.ObjectTypeDefinition
objectTypeDefinition = AST.ObjectTypeDefinition
  <$> optDesc
  <*  tok "type"
  <*> nameParser
  <*> optempty interfaces
  <*> optempty directives
  <*> fieldDefinitions

interfaces :: Parser [AST.Name]
interfaces = tok "implements" *> many1 nameParser

fieldDefinitions :: Parser [AST.FieldDefinition]
fieldDefinitions = braces $ many1 fieldDefinition

fieldDefinition :: Parser AST.FieldDefinition
fieldDefinition = AST.FieldDefinition
  <$> optDesc
  <*> nameParser
  <*> optempty argumentsDefinition
  <*  tok ":"
  <*> graphQLType
  <*> optempty directives

argumentsDefinition :: Parser AST.ArgumentsDefinition
argumentsDefinition = parens $ many1 inputValueDefinition

interfaceTypeDefinition :: PossibleTypes pos => Parser (AST.InterfaceTypeDefinition pos)
interfaceTypeDefinition = AST.InterfaceTypeDefinition
  <$> optDesc
  <*  tok "interface"
  <*> nameParser
  <*> optempty directives
  <*> fieldDefinitions
  <*> possibleTypes

unionTypeDefinition :: Parser AST.UnionTypeDefinition
unionTypeDefinition = AST.UnionTypeDefinition
  <$> optDesc
  <*  tok "union"
  <*> nameParser
  <*> optempty directives
  <*  tok "="
  <*> unionMembers

unionMembers :: Parser [AST.Name]
unionMembers = nameParser `sepBy1` tok "|"

scalarTypeDefinition :: Parser AST.ScalarTypeDefinition
scalarTypeDefinition = AST.ScalarTypeDefinition
  <$> optDesc
  <*  tok "scalar"
  <*> nameParser
  <*> optempty directives

enumTypeDefinition :: Parser AST.EnumTypeDefinition
enumTypeDefinition = AST.EnumTypeDefinition
  <$> optDesc
  <*  tok "enum"
  <*> nameParser
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
enumValue = AST.EnumValue <$> nameParser

inputObjectTypeDefinition :: Parser AST.InputObjectTypeDefinition
inputObjectTypeDefinition = AST.InputObjectTypeDefinition
  <$> optDesc
  <*  tok "input"
  <*> nameParser
  <*> optempty directives
  <*> inputValueDefinitions

inputValueDefinitions :: Parser [AST.InputValueDefinition]
inputValueDefinitions = braces $ many1 inputValueDefinition

inputValueDefinition :: Parser AST.InputValueDefinition
inputValueDefinition = AST.InputValueDefinition
  <$> optDesc
  <*> nameParser
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
    Just c  -> guard (not (isNonFirstChar c))

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

nameParser :: AT.Parser AST.Name
nameParser =
  AST.unsafeMkName <$> tok ((<>) <$> AT.takeWhile1 isFirstChar
                                 <*> AT.takeWhile isNonFirstChar)
{-# INLINE nameParser #-}

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
