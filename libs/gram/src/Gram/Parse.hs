-- | Parsing gram notation to Pattern Subject.
--
-- This module provides functions to parse gram notation text format into
-- Pattern Subject data structures. The parsing handles all aspects of
-- gram notation including:
--
-- * Top-level optional record
-- * Patterns with subjects (square brackets) and paths (nodes/relationships)
-- * Subjects with nested elements after pipe (ONLY structure with nesting)
-- * Nodes (round parentheses) - atomic, no nested elements
-- * Relationships connecting nodes
-- * All value types (standard and extended)
--
-- == Grammar Structure
--
-- gram: optional(record) + repeat(pattern)
-- pattern: optional(annotations) + commaSep1(pattern_element)
-- pattern_element: subject | path
-- subject: [attributes? | sub_pattern?]  -- ONLY structure with nested elements
-- node: (attributes?)  -- NO nested elements
-- relationship: node + relationship_kind + path
--
-- == Examples
--
-- Node: "(n:Person {name:\"Alice\"})"
-- Subject: "[a:Subject {k:\"v\"} | (b), (c)]"
-- Relationship: "()-->()"
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
import Text.Megaparsec (Parsec, parse, eof, optional, try, lookAhead, many, manyTill, some, sepBy, sepBy1, (<|>), satisfy)
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
data ParseError = ParseError String
  deriving (Eq, Show)

-- | Convert Megaparsec parse error to ParseError.
convertError :: ParseErrorBundle String Void -> ParseError
convertError bundle = ParseError (errorBundlePretty bundle)

-- | Strip comments from gram notation string.
-- Handles both line comments (//) and end-of-line comments.
stripComments :: String -> String
stripComments = unlines . filter (not . null) . map stripLineComment . lines
  where
    stripLineComment line = case findComment line of
      Nothing -> line
      Just idx -> take idx line
    findComment s = findComment' s 0 False
    findComment' [] _ _ = Nothing
    findComment' ('"' : xs) idx inString = findComment' xs (idx + 1) (not inString)
    findComment' ('\\' : _ : xs) idx inString = findComment' xs (idx + 2) inString  -- Skip escaped char
    findComment' ('/' : '/' : _) idx False = Just idx  -- Found comment, not in string
    findComment' (_ : xs) idx inString = findComment' xs (idx + 1) inString

-- | Parse optional whitespace (spaces and tabs only, no newlines).
optionalSpace :: Parser ()
optionalSpace = void $ many (char ' ' <|> char '\t')

-- | Parse optional whitespace including newlines (for use in maps and records).
optionalSpaceWithNewlines :: Parser ()
optionalSpaceWithNewlines = void $ many (char ' ' <|> char '\t' <|> char '\n' <|> char '\r')

-- | Parse a symbol identifier.
parseSymbol :: Parser Symbol
parseSymbol = do
  first <- satisfy (\c -> isAlphaNum c || c == '_')
  rest <- many (satisfy (\c -> isAlphaNum c || c == '_' || c == '-' || c == '.' || c == '@'))
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

-- | Parse a string value (double-quoted).
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
  -- Wrap parseScalarValue in try for proper backtracking
  values <- sepBy (try parseScalarValue) (optionalSpace >> char ',' >> optionalSpace)
  optionalSpace
  void $ char ']'
  return $ VArray values

-- | Parse a map value.
parseMap :: Parser Value
parseMap = do
  void $ char '{'
  optionalSpaceWithNewlines
  -- Wrap parseMapping in try for proper backtracking
  pairs <- sepBy (try parseMapping) (optionalSpaceWithNewlines >> char ',' >> optionalSpaceWithNewlines)
  optionalSpaceWithNewlines
  void $ char '}'
  return $ VMap (Map.fromList pairs)
  where
    parseMapping = do
      key <- parseIdentifier
      void $ optionalSpaceWithNewlines >> char ':' >> optionalSpaceWithNewlines
      value <- parseScalarValue
      -- Consume any trailing whitespace after the value
      optionalSpaceWithNewlines
      return (identifierToString key, value)

-- | Parse a range value.
-- Supports: 1..10, 1..., ...10, ...
-- Note: Must be tried before parseInteger in parseValue to avoid consuming the number
parseRange :: Parser Value
parseRange = do
  -- Try to parse lower bound (optional)
  -- First check if we start with dots (for ...10 case)
  startsWithDots <- lookAhead (optional (try (string "..")))
  lower <- if startsWithDots == Just ".."
    then return Nothing  -- We start with dots, so no lower bound
    else do
      -- Parse the integer part first (without decimal, to avoid consuming the first dot)
      -- Then check if dots follow to confirm this is a range
      sign <- optional (char '-')
      intPart <- some digitChar
      -- Check if dots follow - if not, this is not a range, so fail (wrapped in try in parseValue)
      hasDots <- lookAhead (optional (try (string "..")))
      if hasDots == Just ".."
        then do
          -- Dots follow, so this is a range - use the integer part
          let num = read intPart :: Double
          let numWithSign = if sign == Just '-' then -num else num
          return (Just numWithSign)
        else fail "not a range"  -- No dots, fail so parseValue can try other alternatives
  -- Parse dots: ".." (closed) or "..." (open on one or both sides)
  firstDot <- char '.'
  secondDot <- char '.'
  hasThirdDot <- optional (char '.')
  if hasThirdDot == Just '.'
    then do
      -- Three dots: "..." - upper bound is optional
      upper <- optional (try parseRangeDouble)
      return $ VRange (RangeValue lower upper)
    else do
      -- Two dots: ".." - upper bound is required if lower is present, optional if not
      upper <- if lower == Nothing
        then optional (try parseRangeDouble)
        else Just <$> parseRangeDouble
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

-- | Parse a scalar value (for arrays and maps).
parseScalarValue :: Parser Value
parseScalarValue = 
  try parseRange <|>
  try parseMeasurement <|>
  try (VDecimal <$> parseDecimal) <|>
  try (VInteger <$> parseInteger) <|>
  try (VBoolean <$> parseBoolean) <|>
  try parseTaggedString <|>
  try (VString <$> parseString) <|>
  (VSymbol . quoteSymbol <$> parseSymbol)
  where
    quoteSymbol (Symbol s) = s

-- | Parse a value (all types).
parseValue :: Parser Value
parseValue = 
  try parseRange <|>
  try parseMeasurement <|>
  try (VDecimal <$> parseDecimal) <|>
  try (VInteger <$> parseInteger) <|>
  try (VBoolean <$> parseBoolean) <|>
  try parseTaggedString <|>
  try (VString <$> parseString) <|>
  try parseArray <|>
  try parseMap <|>
  (VSymbol . quoteSymbol <$> parseSymbol)
  where
    quoteSymbol (Symbol s) = s

-- | Identifier type (symbol, string, or integer).
data Identifier = IdentSymbol Symbol | IdentString String | IdentInteger Integer

identifierToString :: Identifier -> String
identifierToString (IdentSymbol (Symbol s)) = s
identifierToString (IdentString s) = s
identifierToString (IdentInteger i) = show i

-- | Parse an identifier (symbol, string, or integer).
parseIdentifier :: Parser Identifier
parseIdentifier = 
  try (IdentString <$> parseString) <|>
  try (IdentInteger <$> parseInteger) <|>
  (IdentSymbol <$> parseSymbol)

-- | Parse label separator (`:` or `::`).
parseLabelSeparator :: Parser ()
parseLabelSeparator = (try (string "::" >> return ()) <|> (char ':' >> return ()))

-- | Parse labels.
parseLabels :: Parser (Set String)
parseLabels = do
  firstLabel <- parseLabel
  restLabels <- many parseLabel
  return $ Set.fromList (firstLabel : restLabels)
  where
    parseLabel = do
      parseLabelSeparator
      lbl <- parseSymbol
      return $ quoteSymbol lbl
    quoteSymbol (Symbol s) = s

-- | Parse a property record.
-- Note: Callers should wrap in try if backtracking is needed.
parsePropertyRecord :: Parser (Map String Value)
parsePropertyRecord = do
  void $ char '{'
  optionalSpaceWithNewlines
  -- Use sepBy to allow empty records (empty list)
  -- Wrap parseProperty in try to allow proper backtracking if parsing fails
  pairs <- sepBy (try parseProperty) (optionalSpaceWithNewlines >> char ',' >> optionalSpaceWithNewlines)
  optionalSpaceWithNewlines
  void $ char '}'
  return $ Map.fromList pairs
  where
    parseProperty = do
      key <- parseIdentifier
      optionalSpaceWithNewlines
      parseLabelSeparator
      optionalSpaceWithNewlines
      value <- parseValue
      -- Consume any trailing whitespace after the value
      optionalSpaceWithNewlines
      return (identifierToString key, value)
  
-- | Parse a property (for use in record-only case).
parsePropertyForRecord :: Parser (String, Value)
parsePropertyForRecord = do
  key <- parseIdentifier
  optionalSpace
  parseLabelSeparator
  optionalSpace
  value <- parseValue
  return (identifierToString key, value)
  where
    identifierToString ident = case ident of
      IdentSymbol (Symbol s) -> s
      IdentString s -> s
      IdentInteger i -> show i

-- | Parse attributes (identifier, labels, record - all optional, various combinations).
-- 
-- Architecture: Sequential parsing with explicit lookahead dispatch.
-- Use lookahead to determine what we're parsing, avoiding fragile alternative ordering:
-- 1. If starts with '{', parse record-only
-- 2. If starts with ':', parse labels-first (labels or labels+record)
-- 3. Otherwise, parse identifier-first (identifier, identifier+labels, identifier+record, or all three)
--
-- This approach is deterministic and doesn't depend on Megaparsec's alternative ordering.
parseAttributes :: Parser (Symbol, Set String, Map String Value)
parseAttributes = do
  -- Use lookahead to determine the structure
  -- Note: Caller should handle leading whitespace
  nextChar <- lookAhead (satisfy (const True))
  
  case nextChar of
    '{' -> do
      -- Record-only case - wrap in try for proper backtracking
      props <- try parsePropertyRecord
      return (Symbol "", Set.empty, props)
    ':' -> do
      -- Labels-first case (labels or labels+record)
      lbls <- parseLabels
      optionalSpace
      props <- optional (try parsePropertyRecord)
      return (Symbol "", lbls, maybe Map.empty id props)
    _ -> do
      -- Identifier-first case (identifier, identifier+labels, identifier+record, or all three)
      ident <- parseIdentifier
      optionalSpace
      -- Check for labels
      hasLabels <- lookAhead (optional (try (char ':')))
      lbls <- if hasLabels == Just ':'
        then do
          labels <- parseLabels
          optionalSpace  -- Consume whitespace after labels
          return labels
        else return Set.empty
      optionalSpace
      -- Check for record
      hasRecord <- lookAhead (optional (char '{'))
      props <- if hasRecord == Just '{'
        then parsePropertyRecord
        else return Map.empty
      return (identifierToSymbol ident, lbls, props)
  where
    identifierToSymbol (IdentSymbol s) = s
    identifierToSymbol (IdentString s) = Symbol s
    identifierToSymbol (IdentInteger i) = Symbol (show i)

-- | Parse a node (round parentheses) - NO nested elements.
parseNode :: Parser (Pattern Subject)
parseNode = do
  void $ char '('
  optionalSpace
  -- Parse attributes (optional - can be empty node)
  -- parseAttributes handles all cases: record-only, labels-first, identifier-first
  -- It uses lookahead internally to determine the structure
  attrs <- try parseAttributes <|> return (Symbol "", Set.empty, Map.empty)
  optionalSpace
  void $ char ')'
  let (symbol, labels, properties) = attrs
  return $ Pattern (Subject symbol labels properties) []

-- | Parse a reference (identifier only).
parseReference :: Parser (Pattern Subject)
parseReference = do
  ident <- parseIdentifier
  -- Consume any trailing whitespace after the identifier
  optionalSpace
  return $ Pattern (Subject (identifierToSymbol ident) Set.empty Map.empty) []
  where
    identifierToSymbol (IdentSymbol s) = s
    identifierToSymbol (IdentString s) = Symbol s
    identifierToSymbol (IdentInteger i) = Symbol (show i)

-- | Parse relationship kind (arrows).
parseRelationshipKind :: Parser String
parseRelationshipKind = 
  try (string "<->" >> return "<->") <|>
  try (string "<--" >> return "<--") <|>
  try (string "-->" >> return "-->") <|>
  try (string "<=>" >> return "<=>") <|>
  try (string "<=" >> return "<=") <|>
  try (string "=>" >> return "=>") <|>
  try (string "<~>" >> return "<~>") <|>
  try (string "<~~" >> return "<~~") <|>  -- Squiggle arrow left
  try (string "~~>" >> return "~~>") <|>  -- Squiggle arrow right
  try (string "<~" >> return "<~") <|>
  try (string "~>" >> return "~>") <|>
  try (string "~~" >> return "~~") <|>
  try (string "==" >> return "==") <|>
  (string "--" >> return "--")

-- | Parse relationship with optional attributes in square brackets.
parseRelationshipWithAttributes :: Parser (Pattern Subject)
parseRelationshipWithAttributes = do
  void $ char '['
  optionalSpace
  attrs <- optional parseAttributes
  optionalSpace
  void $ char ']'
  let (symbol, labels, properties) = maybe (Symbol "", Set.empty, Map.empty) id attrs
  return $ Pattern (Subject symbol labels properties) []

-- | Parse a relationship.
parseRelationship :: Parser (Pattern Subject)
parseRelationship = do
  left <- parseNode
  optionalSpace
  -- Check for relationship attributes in square brackets
  relAttrs <- optional (try parseRelationshipWithAttributes)
  optionalSpace
  kind <- parseRelationshipKind
  optionalSpace
  right <- parsePath
  -- Create a relationship pattern: left node as main, right node as element
  -- The relationship arrow kind is consumed but not stored in the data structure
  return $ Pattern (value left) [right]

-- | Parse a path (relationship or node).
parsePath :: Parser (Pattern Subject)
parsePath = try parseRelationship <|> parseNode

-- | Parse sub-pattern element (subject, path, or reference).
parseSubPatternElement :: Parser (Pattern Subject)
parseSubPatternElement = try parseSubject <|> try parsePath <|> try parseReference

-- | Parse a subject (square brackets) - ONLY structure with nested elements after pipe.
parseSubject :: Parser (Pattern Subject)
parseSubject = do
  void $ char '['
  optionalSpace
  -- Parse attributes (optional)
  attrs <- optional parseAttributes
  optionalSpace
  -- Check for nested elements after pipe
  nested <- optional (do
    void $ char '|'
    optionalSpace
    -- Parse comma-separated elements (at least one required by grammar: commaSep1)
    -- Wrap parseSubPatternElement in try for proper backtracking
    elements <- sepBy1 (try parseSubPatternElement) (optionalSpace >> char ',' >> optionalSpace)
    -- Consume any trailing whitespace after the last element
    optionalSpace
    return elements)
  optionalSpace
  void $ char ']'
  let (symbol, labels, properties) = maybe (Symbol "", Set.empty, Map.empty) id attrs
  let nestedElements = maybe [] id nested
  return $ Pattern (Subject symbol labels properties) nestedElements

-- | Parse a pattern element (subject or path).
parsePatternElement :: Parser (Pattern Subject)
parsePatternElement = try parseSubject <|> parsePath

-- | Parse a pattern (comma-separated elements).
-- A pattern must have at least one element.
parsePattern :: Parser (Pattern Subject)
parsePattern = do
  optionalSpace
  -- Parse first element (required)
  first <- parsePatternElement
  -- Parse additional elements (optional, comma-separated)
  rest <- many (do
    optionalSpace
    void $ char ','
    optionalSpace
    parsePatternElement)
  -- For a single element, return it as-is
  -- For multiple elements, use first as main, rest as nested
  if null rest
    then return first
    else return $ Pattern (value first) (elements first ++ rest)

-- | Parse gram notation to Pattern Subject.
--
-- Top-level structure: optional(record) + repeat(pattern)
-- For now, we parse the first pattern as the main result.
-- If only a record is present, we create an empty pattern with that record as properties.
fromGram :: String -> Either ParseError (Pattern Subject)
fromGram input = do
  let stripped = stripComments input
  case parse (do
    optionalSpace
    -- Check if we start with a record (starts with '{')
    rootRecord <- optional (try parsePropertyRecord)
    optionalSpace
    -- Parse first pattern (required if no record, optional if record is present)
    -- When we have a record, we still need to try to parse a pattern that follows
    -- Use try to allow backtracking if pattern parsing fails
    firstPattern <- if rootRecord == Nothing
      then Just <$> parsePattern  -- Pattern is required if no record
      else optional (try parsePattern)  -- Pattern is optional if we have a record, but try to parse it
    -- Parse additional patterns on separate lines
    -- Handle both "record followed by pattern" and "multiple patterns on separate lines"
    additionalPatterns <- do
      optionalSpaceWithNewlines
      -- Try to parse more patterns (many will stop gracefully on failure)
      many (try (do
        -- Check if there's a pattern start character
        nextChar <- lookAhead (satisfy (const True))
        if nextChar == '(' || nextChar == '[' || isSymbolStart nextChar
          then do
            optionalSpaceWithNewlines
            parsePattern
          else fail "no pattern"))
    -- Allow trailing whitespace (including newlines) before eof
    optionalSpaceWithNewlines
    eof
    -- If we have a pattern, use it; otherwise create pattern from root record
    -- If we have both a record and patterns, the record properties go on the main subject,
    -- and the patterns become elements
    case (rootRecord, firstPattern, additionalPatterns) of
      (Just props, Just p, rest) -> do
        -- Record properties go on main subject, patterns become elements
        return $ Pattern (Subject (Symbol "") Set.empty props) (p : rest)
      (Just props, Nothing, additional) -> 
        -- Record with no first pattern, but may have additional patterns (from newline)
        return $ Pattern (Subject (Symbol "") Set.empty props) additional
      (Nothing, Just p, rest) -> 
        if null rest
          then return p
          else return $ Pattern (value p) (elements p ++ rest)
      (Nothing, Nothing, _) -> return $ Pattern (Subject (Symbol "") Set.empty Map.empty) []
    ) "gram" stripped of
    Left err -> Left (convertError err)
    Right p -> Right p
