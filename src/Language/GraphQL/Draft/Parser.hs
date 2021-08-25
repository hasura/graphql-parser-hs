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
  , blockString
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
import           Data.Maybe                    (fromMaybe)

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
      AST.VVariable    <$> variable
  <|> (fmap (either AST.VFloat AST.VInt) number <?> "number")
  <|> AST.VNull    <$  literal "null"
  <|> AST.VBoolean <$> booleanLiteral
  <|> AST.VString  <$> blockString
  <|> AST.VString  <$> stringLiteral
  -- `true` and `false` have been tried before, so we can safely proceed with the enum parser
  <|> AST.VEnum    <$> (fmap AST.EnumValue nameParser <?> "name")
  <|> AST.VList    <$> listLiteral
  <|> AST.VObject  <$> objectLiteral
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
    go :: Bool -> Char -> Maybe Bool
    go previousWasEscapingCharacter current
      -- if the previous character was an escaping character, we skip this one
      | previousWasEscapingCharacter = Just False
      -- otherwise, if we find an unescaped quote, we've reached the end
      | current == '"' = Nothing
      -- otherwise, we continue, and track whether the current character is an escaping backslash
      | otherwise = Just $ current == backslash
      where backslash = '\\'

    -- | Unescape a string.
    --
    -- Turns out this is really tricky, so we're going to cheat by
    -- reconstructing a literal string (by putting quotes around it) and
    -- delegating all the hard work to Aeson.
    unescapeText :: Text -> Parser Text
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

typeDefinition :: Parser (AST.TypeDefinition () AST.InputValueDefinition)
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

objectTypeDefinition :: Parser (AST.ObjectTypeDefinition AST.InputValueDefinition)
objectTypeDefinition = AST.ObjectTypeDefinition
  <$> optDesc
  <*  whiteSpace
  <*  tok "type"
  <*> nameParser
  <*> optempty interfaces
  <*> optempty directives
  <*> fieldDefinitions

interfaces :: Parser [AST.Name]
interfaces = tok "implements" *> nameParser `sepBy1` tok "&"

fieldDefinitions :: Parser [AST.FieldDefinition AST.InputValueDefinition]
fieldDefinitions = braces $ many1 fieldDefinition

fieldDefinition :: Parser (AST.FieldDefinition AST.InputValueDefinition)
fieldDefinition = AST.FieldDefinition
  <$> optDesc
  <*  whiteSpace
  <*> nameParser
  <*> optempty argumentsDefinition
  <*  tok ":"
  <*> graphQLType
  <*> optempty directives

argumentsDefinition :: Parser (AST.ArgumentsDefinition AST.InputValueDefinition)
argumentsDefinition = parens $ many1 inputValueDefinition

interfaceTypeDefinition :: PossibleTypes pos => Parser (AST.InterfaceTypeDefinition pos AST.InputValueDefinition)
interfaceTypeDefinition = AST.InterfaceTypeDefinition
  <$> optDesc
  <*  whiteSpace
  <*  tok "interface"
  <*> nameParser
  <*> optempty directives
  <*> fieldDefinitions
  <*> possibleTypes

unionTypeDefinition :: Parser AST.UnionTypeDefinition
unionTypeDefinition = AST.UnionTypeDefinition
  <$> optDesc
  <*  whiteSpace
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
  <*  whiteSpace
  <*  tok "scalar"
  <*> nameParser
  <*> optempty directives

enumTypeDefinition :: Parser AST.EnumTypeDefinition
enumTypeDefinition = AST.EnumTypeDefinition
  <$> optDesc
  <*  whiteSpace
  <*  tok "enum"
  <*> nameParser
  <*> optempty directives
  <*> enumValueDefinitions

enumValueDefinitions :: Parser [AST.EnumValueDefinition]
enumValueDefinitions = braces $ many1 enumValueDefinition

enumValueDefinition :: Parser AST.EnumValueDefinition
enumValueDefinition = AST.EnumValueDefinition
  <$> optDesc
  <*  whiteSpace
  <*> enumValue
  <*> optempty directives

-- TODO: should not be one of true/false/null
enumValue :: Parser AST.EnumValue
enumValue = AST.EnumValue <$> nameParser

inputObjectTypeDefinition :: Parser (AST.InputObjectTypeDefinition AST.InputValueDefinition)
inputObjectTypeDefinition = AST.InputObjectTypeDefinition
  <$> optDesc
  <*  whiteSpace
  <*  tok "input"
  <*> nameParser
  <*> optempty directives
  <*> inputValueDefinitions

inputValueDefinitions :: Parser [AST.InputValueDefinition]
inputValueDefinitions = braces $ many1 inputValueDefinition

inputValueDefinition :: Parser AST.InputValueDefinition
inputValueDefinition = AST.InputValueDefinition
  <$> optDesc
  <*  whiteSpace
  <*> nameParser
  <*  tok ":"
  <*> graphQLType
  <*> optional defaultValue
  <*> optempty directives

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


data BlockState
  = BlockWasNeverClosed
  | Escaped Int 
  | Closing Int
  | Normal
  | Done

-- | Parses strings delimited by triple quotes. 
-- http://spec.graphql.org/June2018/#sec-String-Value
blockString :: Parser Text
blockString = do

  -- TODO this need to be replaced with AT.scan
  --lines_ <- T.lines . T.pack <$> AT.manyTill AT.anyChar tripleQuotesFoo <?> "the body of a triple quoted string"

  _ <- tripleQuotes <?> "opening triple quotes"
  lines_ <- AT.runScanner Normal scanner >>= \case
    (_,BlockWasNeverClosed) -> fail ""
    (lines_,Done)           -> return (T.lines (T.dropEnd 3 lines_))
    (lines_,bs)             -> return (T.lines lines_)
  let headline = if lines_ == [] then "" else head lines_
  let tail_ = drop 1 lines_ -- not tail
  let commonIndentation = foldr min maxBound (countIndentation <$> tail_)
  let rlines = foldr (removeCommonIndentation commonIndentation) Nothing tail_
  return $ case rlines of
    Nothing -> headline
    Just reformatedLines -> rebuild (sanitize $ headline:reformatedLines)
 where

  -- also known as go
  scanner :: BlockState -> Char -> Maybe BlockState
  scanner s ch = 
    case s of
      Done -> Nothing
      Normal ->
        if ch == '\\' 
        then Just (Escaped 0)
        else if ch == '"' then Just (Closing 1) else Just Normal
      Closing 1 -> if ch == '"' then Just (Closing 2) else Just Normal
      Closing 2 -> if ch == '"' then Just Done else Just Normal
      Escaped 0 -> if ch == '"' then Just (Escaped 1) else Just Normal
      Escaped 1 -> if ch == '"' then Just (Escaped 2) else Just Normal
      Escaped 2 -> Just Normal

  rebuild :: [Text] -> Text
  rebuild = fromMaybe "" . fmap fst . T.unsnoc . T.unlines
  sanitize :: [Text] -> [Text]
  sanitize = dropWhileEnd' onlyWhiteSpace  . dropWhile onlyWhiteSpace
  onlyWhiteSpace :: Text -> Bool
  onlyWhiteSpace t = T.all ws t
  removeCommonIndentation :: Int -> Text -> Maybe [Text] -> Maybe [Text] 
  removeCommonIndentation smallest a x = 
    let new = T.drop smallest a
     in case x of
      Nothing -> Just [new]
      Just acc -> Just $ new:acc
  countIndentation :: Text -> Int
  countIndentation = fromMaybe maxBound . T.findIndex (not . ws)
  tripleQuotes = AT.string "\"\"\""
  tripleQuotesFoo = do
    AT.option False (AT.string "\\" >> AT.string "\"\"\"" >> pure True) >>= \case
      True -> fail ""
      False -> tripleQuotes

-- whitespace
ws :: Char -> Bool
ws c = c == ' ' || c == '\t'

-- copied from https://hackage.haskell.org/package/extra-1.7.9/docs/src/Data.List.Extra.html
dropWhileEnd' :: (a -> Bool) -> [a] -> [a]
dropWhileEnd' p = foldr (\x xs -> if null xs && p x then [] else x : xs) []
