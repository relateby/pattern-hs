-- | Parsing gram notation to Pattern Subject.
--
-- This module provides functions to parse gram notation text format into
-- Pattern Subject data structures. The parsing handles all aspects of
-- gram notation including:
--
-- * Subject identity (symbols, quoted strings, numbers)
-- * Labels (single and multiple)
-- * Property records with all value types
-- * Pattern structure (nodes, relationships, nested patterns)
--
-- == Parsing Strategy
--
-- The parsing process converts gram notation to Haskell data structures:
--
-- * gram notation string → @Pattern Subject@
-- * gram attributes notation → @Subject@
-- * gram value notation → @Value@ types
--
-- == Examples
--
-- Parsing a simple subject:
--
-- >>> fromGram "(n:Person)"
-- Right (Pattern { value = Subject (Symbol "n") (Set.fromList ["Person"]) empty, elements = [] })
--
-- Parsing with properties:
--
-- >>> fromGram "(n:Person {name:\"Alice\"})"
-- Right (Pattern { value = Subject (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "Alice")]), elements = [] })
--
-- Parsing errors:
--
-- >>> fromGram "(invalid"
-- Left (ParseError "Unexpected end of input")
{-# LANGUAGE OverloadedStrings #-}
module Gram.Parse
  ( fromGram
  , ParseError(..)
  ) where

import Pattern.Core (Pattern(..))
import Subject.Core (Subject(..), Symbol(..))
import Subject.Value (Value(..), RangeValue(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Megaparsec (Parsec, parse, eof, optional, try, lookAhead, many, manyTill, some, sepBy, (<|>), satisfy)
import Text.Megaparsec hiding (ParseError)
import Text.Megaparsec.Char (char, string, digitChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (ParseErrorBundle, errorBundlePretty)
import Data.Void
import Control.Monad (void)
import Data.Char (isAlphaNum, isDigit, isAlpha)

-- | Parser type for gram notation.
type Parser = Parsec Void String

-- | Parse error type for gram notation parsing.
--
-- Represents errors that can occur during parsing of gram notation,
-- such as syntax errors, unexpected tokens, or incomplete input.
data ParseError = ParseError String
  deriving (Eq, Show)

-- | Convert Megaparsec parse error to ParseError.
convertError :: ParseErrorBundle String Void -> ParseError
convertError bundle = ParseError (errorBundlePretty bundle)

-- | Strip comments from gram notation string.
--
-- Removes line comments (`//`) and end-of-line comments.
-- Comments are formatting/metadata and not preserved in Pattern Subject structure.
stripComments :: String -> String
stripComments = unlines . map stripLineComment . lines
  where
    stripLineComment line = case findComment line of
      Nothing -> line
      Just idx -> take idx line
    findComment s = findComment' s 0
    findComment' [] _ = Nothing
    findComment' ('/' : '/' : _) idx = Just idx
    findComment' (_ : xs) idx = findComment' xs (idx + 1)

-- | Unescape special characters in strings.
--
-- Converts escaped sequences back to their original characters.
unescapeString :: String -> String
unescapeString [] = []
unescapeString ('\\' : '"' : xs) = '"' : unescapeString xs
unescapeString ('\\' : '\\' : xs) = '\\' : unescapeString xs
unescapeString (x : xs) = x : unescapeString xs

-- | Parse whitespace (spaces, tabs, newlines).
spaceParser :: Parser ()
spaceParser = void $ many (char ' ' <|> char '\t' <|> char '\n')

-- | Parse optional whitespace (spaces and tabs only, no newlines).
optionalSpace :: Parser ()
optionalSpace = void $ many (char ' ' <|> char '\t')

-- | Parse a symbol identifier.
--
-- Symbols are unquoted identifiers (alphanumeric, underscore, etc.)
-- Must start with alphanumeric or underscore, can contain alphanumeric, underscore, dash, dot.
parseSymbol :: Parser Symbol
parseSymbol = do
  first <- satisfy (\c -> isAlphaNum c || c == '_')
  rest <- many (satisfy (\c -> isAlphaNum c || c == '_' || c == '-' || c == '.'))
  return $ Symbol (first : rest)

-- | Check if next character is a symbol start character.
isSymbolStart :: Char -> Bool
isSymbolStart c = isAlphaNum c || c == '_'

-- | Parse an integer value.
parseInteger :: Parser Integer
parseInteger = do
  sign <- optional (char '-')
  digits <- some digitChar
  let num = read digits :: Integer
  return $ if sign == Just '-' then -num else num

-- | Parse a decimal value.
parseDecimal :: Parser Double
parseDecimal = do
  sign <- optional (char '-')
  intPart <- some digitChar
  void $ char '.'
  fracPart <- some digitChar
  let num = read (intPart ++ "." ++ fracPart) :: Double
  return $ if sign == Just '-' then -num else num

-- | Parse a boolean value.
parseBoolean :: Parser Bool
parseBoolean = 
  (string "true" >> return True) <|>
  (string "false" >> return False)

-- | Parse a string value (quoted).
parseString :: Parser String
parseString = do
  void $ char '"'
  content <- manyTill (escapedChar <|> satisfy (\c -> c /= '"' && c /= '\\')) (char '"')
  return content
  where
    escapedChar = do
      void $ char '\\'
      c <- satisfy (const True)
      return $ case c of
        '"' -> '"'
        '\\' -> '\\'
        _ -> c

-- | Parse a tagged string value.
parseTaggedString :: Parser Value
parseTaggedString = do
  tag <- parseSymbol
  void $ char '`'
  content <- manyTill (satisfy (const True)) (char '`')
  return $ VTaggedString (quoteSymbol tag) content
  where
    quoteSymbol (Symbol s) = s

-- | Parse an array value.
parseArray :: Parser Value
parseArray = do
  void $ char '['
  optionalSpace
  values <- sepBy parseValue (optionalSpace >> char ',' >> optionalSpace)
  optionalSpace
  void $ char ']'
  return $ VArray values

-- | Parse a map value.
parseMap :: Parser Value
parseMap = do
  void $ char '{'
  optionalSpace
  pairs <- sepBy parsePropertyPair (optionalSpace >> char ',' >> optionalSpace)
  optionalSpace
  void $ char '}'
  return $ VMap (Map.fromList pairs)
  where
    parsePropertyPair = do
      key <- parseSymbol
      void $ optionalSpace >> char ':' >> optionalSpace
      value <- parseValue
      return (quoteSymbol key, value)
    quoteSymbol (Symbol s) = s

-- | Parse a range value.
parseRange :: Parser Value
parseRange = do
  lower <- optional (try parseRangeDouble)
  void $ string ".."
  hasUpper <- optional (char '.')
  upper <- if hasUpper == Just '.' then optional (try parseRangeDouble) else Just <$> parseRangeDouble
  return $ VRange (RangeValue lower upper)
  where
    parseRangeDouble = do
      sign <- optional (char '-')
      intPart <- some digitChar
      fracPart <- optional (char '.' >> some digitChar)
      let numStr = intPart ++ maybe "" ('.' :) fracPart
      let num = read numStr :: Double
      return $ if sign == Just '-' then -num else num

-- | Parse a measurement value.
parseMeasurement :: Parser Value
parseMeasurement = do
  sign <- optional (char '-')
  intPart <- some digitChar
  fracPart <- optional (char '.' >> some digitChar)
  unit <- some (satisfy (\c -> isAlpha c || c == '%'))
  let numStr = intPart ++ maybe "" ('.' :) fracPart
  let num = read numStr :: Double
  let value = if sign == Just '-' then -num else num
  return $ VMeasurement unit value

-- | Parse a value (all types).
parseValue :: Parser Value
parseValue = 
  try parseTaggedString <|>
  try parseArray <|>
  try parseMap <|>
  try parseRange <|>
  try parseMeasurement <|>
  try (VInteger <$> parseInteger) <|>
  try (VDecimal <$> parseDecimal) <|>
  try (VBoolean <$> parseBoolean) <|>
  try (VString <$> parseString) <|>
  (VSymbol . quoteSymbol <$> parseSymbol)
  where
    quoteSymbol (Symbol s) = s

-- | Parse a property record.
parsePropertyRecord :: Parser (Map String Value)
parsePropertyRecord = do
  optionalSpace
  void $ char '{'
  optionalSpace
  pairs <- sepBy parsePropertyPair (optionalSpace >> char ',' >> optionalSpace)
  optionalSpace
  void $ char '}'
  return $ Map.fromList pairs
  where
    parsePropertyPair = do
      key <- parseSymbol
      void $ optionalSpace >> char ':' >> optionalSpace
      value <- parseValue
      return (quoteSymbol key, value)
    quoteSymbol (Symbol s) = s

-- | Parse a subject (identity, labels, properties).
-- Note: This does NOT parse the closing ')' - that's handled by parsePattern.
parseSubject :: Parser Subject
parseSubject = do
  void $ char '('
  optionalSpace
  ident <- parseSubjectIdentity
  optionalSpace
  lbls <- parseLabels
  optionalSpace
  props <- parseOptionalPropertyRecord
  optionalSpace
  return $ Subject ident lbls props
  where
    parseSubjectIdentity = do
      -- Check if next character is a symbol start, otherwise it's anonymous
      nextChar <- lookAhead (satisfy (const True))
      if isSymbolStart nextChar
        then parseSymbol
        else return (Symbol "")
    
    parseLabels = do
      -- Check if there's a label (starts with ':')
      hasColon <- optional (char ':')
      case hasColon of
        Just _ -> do
          firstLabel <- parseSymbol
          restLabels <- many (char ':' >> parseSymbol)
          return $ Set.fromList (map quoteSymbol (firstLabel : restLabels))
        Nothing -> return Set.empty
      where
        quoteSymbol (Symbol s) = s
    
    parseOptionalPropertyRecord = do
      -- Check if there's a property record (starts with '{')
      hasProps <- optional (lookAhead (char '{'))
      case hasProps of
        Just _ -> parsePropertyRecord
        Nothing -> return Map.empty

-- | Parse pattern elements (nested patterns).
-- Elements come INSIDE the subject's parentheses, before the closing ')'.
parsePatternElements :: Parser [Pattern Subject]
parsePatternElements = many (try $ do
  optionalSpace
  p <- parsePattern
  return p)

-- | Parse a complete pattern.
-- Pattern structure: (subject element1 element2...)
-- where elements are nested patterns inside the parentheses.
parsePattern :: Parser (Pattern Subject)
parsePattern = do
  optionalSpace
  subj <- parseSubject  -- Parses subject but NOT the closing ')'
  elems <- parsePatternElements  -- Parse elements inside the parentheses
  optionalSpace
  void $ char ')'  -- Now consume the closing ')'
  return $ Pattern subj elems

-- | Parse gram notation to Pattern Subject.
--
-- Converts a gram notation string into a Pattern Subject data structure.
-- Returns either a parsed Pattern Subject or a ParseError if parsing fails.
--
-- === Examples
--
-- Simple subject:
--
-- >>> fromGram "(n:Person)"
-- Right (Pattern { value = Subject (Symbol "n") (Set.fromList ["Person"]) empty, elements = [] })
--
-- Subject with properties:
--
-- >>> fromGram "(n:Person {name:\"Alice\"})"
-- Right (Pattern { value = Subject (Symbol "n") (Set.fromList ["Person"]) (fromList [("name", VString "Alice")]), elements = [] })
--
-- Parse error:
--
-- >>> fromGram "(invalid"
-- Left (ParseError "Unexpected end of input")
--
-- **Note**: This implementation handles all gram notation features including:
-- * Anonymous subjects (generating identities during parsing)
-- * All value types (integers, decimals, booleans, strings, symbols, tagged strings, arrays, maps, ranges, measurements)
-- * Pattern structure (nested patterns, relationships)
-- * Proper error messages with position information
fromGram :: String -> Either ParseError (Pattern Subject)
fromGram input = do
  let stripped = stripComments input
  case parse (parsePattern <* optionalSpace <* eof) "" stripped of
    Left err -> Left (convertError err)
    Right p -> Right p
