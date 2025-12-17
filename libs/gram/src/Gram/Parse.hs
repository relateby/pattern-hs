{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Gram.Parse
-- Description : Parser for gram notation
-- Copyright   : (c) gram-data, 2024
-- License     : BSD3
-- Maintainer  : gram-data
-- Stability   : experimental
--
-- This module provides parsers for gram notation, converting text into
-- Pattern and Subject data structures.
--
-- == String Value Syntax
--
-- The parser supports multiple string formats:
--
-- === Double-quoted strings
--
-- Standard strings with escape sequences:
--
-- @
-- { name: \"Alice\" }
-- @
--
-- === Single-quoted strings
--
-- Literal strings without escape processing:
--
-- @
-- { pattern: \'[a-z]+\' }
-- @
--
-- === Codefence strings (multiline)
--
-- Triple-backtick delimited strings for multiline content:
--
-- @
-- { content: \`\`\`
-- This content spans
-- multiple lines.
-- \`\`\` }
-- @
--
-- === Tagged codefence strings
--
-- Codefence with a tag indicating the content type:
--
-- @
-- { doc: \`\`\`md
-- # Markdown Title
-- Some **bold** text.
-- \`\`\` }
-- @
--
-- Tagged codefences are parsed as 'VTaggedString' values with the tag
-- and content stored separately.
module Gram.Parse
  ( fromGram
  , parseGram
  , ParseError(..)
  ) where

import Gram.CST (Gram(..), AnnotatedPattern(..), PatternElement(..), Path(..), PathSegment(..), Node(..), Relationship(..), SubjectPattern(..), SubjectData(..), Identifier(..), Symbol(..), Annotation(..), Value(..), RangeValue(..))
import qualified Gram.CST as CST
import qualified Gram.Transform as Transform
import qualified Pattern.Core as Core
import qualified Subject.Core as CoreSub
import qualified Subject.Value as V

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Megaparsec (Parsec, parse, eof, optional, try, lookAhead, many, manyTill, some, sepBy, sepBy1, (<|>), satisfy, choice)
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
--
-- Handles line comments starting with @\/\/@. Comments are stripped unless
-- they appear inside string literals (double-quoted, single-quoted, or
-- backtick-quoted) or inside codefence blocks.
--
-- Codefence blocks span multiple lines and start with @\`\`\`@ (optionally
-- followed by a tag) and end with @\`\`\`@ on its own line. Content inside
-- codefences is preserved verbatim, including any @\/\/@ sequences.
stripComments :: String -> String
stripComments input = unlines $ filter (not . null) $ processLines False (lines input)
  where
    -- Process lines while tracking codefence state
    processLines :: Bool -> [String] -> [String]
    processLines _ [] = []
    processLines inCodefence (line:rest)
      | inCodefence = 
          -- Inside codefence content
          if isClosingFence line
            then line : processLines False rest  -- Closing fence, exit codefence
            else line : processLines True rest   -- Keep content verbatim
      | otherwise =
          -- Outside codefence, check for opening fence and strip comments
          let (processed, nowInCodefence) = processLineOutsideCodefence line
          in processed : processLines nowInCodefence rest
    
    -- Check if line is a closing fence (``` possibly with trailing whitespace)
    isClosingFence :: String -> Bool
    isClosingFence s = 
      let trimmed = dropWhile isWhitespace s
      in take 3 trimmed == "```" && all isWhitespace (drop 3 trimmed)
    
    -- Process a line when outside codefence, returns (processed line, entering codefence?)
    processLineOutsideCodefence :: String -> (String, Bool)
    processLineOutsideCodefence line =
      let stripped = stripLineComment line
          -- Check if this line opens a codefence (ends with ``` or ```tag)
          opensCodefence = endsWithCodefenceOpen stripped
      in (stripped, opensCodefence)
    
    -- Check if line ends with opening codefence pattern (``` or ```tag)
    -- This happens when a property value starts a codefence
    endsWithCodefenceOpen :: String -> Bool
    endsWithCodefenceOpen s = 
      let rev = reverse s
          -- Remove trailing whitespace
          trimmed = dropWhile isWhitespace rev
      in case trimmed of
           -- Check for ``` at end (plain codefence) or ```tag (tagged codefence)
           ('`':'`':'`':rest) -> 
             -- After ```, we should have either end of meaningful content
             -- or a tag (alphanumeric) followed by content
             case rest of
               [] -> True  -- Just ``` at end
               (c:_) -> isWhitespace c || isTagChar c
           _ -> False
    
    isWhitespace :: Char -> Bool
    isWhitespace c = c == ' ' || c == '\t'
    
    isTagChar :: Char -> Bool
    isTagChar c = isAlphaNum c || c == '_' || c == '-'
    
    -- Strip line comment from a single line (handles quotes)
    stripLineComment :: String -> String
    stripLineComment line = case findComment line of
      Nothing -> line
      Just idx -> take idx line
    
    findComment :: String -> Maybe Int
    findComment s = findComment' s 0 Nothing
    
    findComment' :: String -> Int -> Maybe Char -> Maybe Int
    findComment' [] _ _ = Nothing
    findComment' ('\\' : _ : xs) idx inString = findComment' xs (idx + 2) inString
    findComment' (c : xs) idx Nothing
      | c == '"'  = findComment' xs (idx + 1) (Just '"')
      | c == '\'' = findComment' xs (idx + 1) (Just '\'')
      | c == '`'  = findComment' xs (idx + 1) (Just '`')
      | c == '/' && take 1 xs == "/" = Just idx
      | otherwise = findComment' xs (idx + 1) Nothing
    findComment' (c : xs) idx (Just q)
      | c == q    = findComment' xs (idx + 1) Nothing
      | otherwise = findComment' xs (idx + 1) (Just q)

-- | Parse optional whitespace (spaces and tabs only, no newlines).
optionalSpace :: Parser ()
optionalSpace = void $ many (char ' ' <|> char '\t')

-- | Parse optional whitespace including newlines.
optionalSpaceWithNewlines :: Parser ()
optionalSpaceWithNewlines = void $ many (char ' ' <|> char '\t' <|> char '\n' <|> char '\r')

-- ... [Value Parsers - largely unchanged, but reused from previous implementation] ...

-- | Parse a symbol identifier.
parseSymbol :: Parser Symbol
parseSymbol = 
  (Symbol <$> try parseBacktickedIdentifier) <|> do
    first <- satisfy (\c -> isAlpha c || c == '_')
    rest <- many (satisfy (\c -> isAlphaNum c || c == '_' || c == '-' || c == '.' || c == '@'))
    return $ Symbol (first : rest)

isSymbolStart :: Char -> Bool
isSymbolStart c = isAlpha c || c == '_'

parseInteger :: Parser Integer
parseInteger = do
  sign <- optional (char '-')
  digits <- some digitChar
  let num = read digits :: Integer
  return $ if sign == Just '-' then -num else num

parseDecimal :: Parser Double
parseDecimal = do
  sign <- optional (char '-')
  intPart <- some digitChar
  void $ char '.'
  fracPart <- some digitChar
  let num = read (intPart ++ "." ++ fracPart) :: Double
  return $ if sign == Just '-' then -num else num

parseBoolean :: Parser Bool
parseBoolean = 
  (string "true" >> return True) <|>
  (string "false" >> return False)

parseString :: Parser String
parseString = parseDoubleQuotedString <|> parseSingleQuotedString

parseDoubleQuotedString :: Parser String
parseDoubleQuotedString = do
  void $ char '"'
  content <- manyTill (escapedChar '"') (char '"')
  return content

parseSingleQuotedString :: Parser String
parseSingleQuotedString = do
  void $ char '\''
  content <- manyTill (escapedChar '\'') (char '\'')
  return content

escapedChar :: Char -> Parser Char
escapedChar quote = 
  satisfy (\x -> x /= quote && x /= '\\') <|> 
  (char '\\' >> anyChar >>= \c -> return (case c of
    '\\' -> '\\'
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'
    x | x == quote -> quote
    x -> x))
  where
    anyChar = satisfy (const True)

parseBacktickedIdentifier :: Parser String
parseBacktickedIdentifier = do
  void $ char '`'
  content <- manyTill (escapedChar '`') (char '`')
  return content

-- | Parse the content of a fenced string (codefence).
--
-- Captures all characters between the opening fence (after newline)
-- and the closing fence. The closing fence must be three backticks
-- at the start of a line (preceded by newline).
--
-- Content may contain:
--
-- * Newlines
-- * Single backticks
-- * Double backticks
-- * Any other characters
--
-- Content may NOT contain three consecutive backticks at line start.
--
-- === Examples
--
-- The content between fences is captured verbatim:
--
-- @
-- \`\`\`
-- Hello World
-- \`\`\`
-- @
--
-- Produces: @"Hello World\\n"@
--
-- === Implementation
--
-- Uses character-by-character parsing to detect the closing fence pattern
-- (newline followed by three backticks). This allows backticks within
-- the content as long as they don't form the closing pattern.
parseFencedContent :: Parser String
parseFencedContent = do
  -- Check if closing fence appears immediately (empty content case)
  closingAtStart <- optional (try (string "```"))
  case closingAtStart of
    Just _ -> return ""  -- Empty content
    Nothing -> go []
  where
    go :: [Char] -> Parser String
    go acc = do
      -- Try to match the closing fence: newline followed by ```
      closingFence <- optional (try (char '\n' >> string "```"))
      case closingFence of
        Just _ -> return (reverse acc)  -- Found closing, return accumulated content
        Nothing -> do
          -- Check for EOF (unclosed fence)
          isEnd <- optional eof
          case isEnd of
            Just _ -> fail "Unclosed codefence: expected closing ```"
            Nothing -> do
              -- Consume one character and continue
              c <- satisfy (const True)
              go (c : acc)

-- | Parse a plain fenced string (codefence without tag).
--
-- Recognizes the syntax:
--
-- @
-- \`\`\`
-- content here
-- can span multiple lines
-- \`\`\`
-- @
--
-- Returns the content between the opening and closing fences as a VString.
-- The opening fence must be immediately followed by a newline.
-- Content may be empty.
--
-- === Examples
--
-- >>> parse parseFencedString "" "```\\nHello World\\n```"
-- Right (VString "Hello World")
--
-- >>> parse parseFencedString "" "```\\n```"
-- Right (VString "")
--
-- === Errors
--
-- Fails if:
--
-- * Opening fence is not followed by newline
-- * Closing fence is missing
parseFencedString :: Parser Value
parseFencedString = do
  -- Match opening fence: ```
  void $ string "```"
  -- Require newline after opening fence (plain codefence has no tag)
  void $ char '\n'
  -- Parse content until closing fence
  content <- parseFencedContent
  return $ V.VString content

-- | Parse a tagged fenced string (codefence with tag).
--
-- Recognizes the syntax:
--
-- @
-- \`\`\`tag
-- content here
-- can span multiple lines
-- \`\`\`
-- @
--
-- The tag must be a valid symbol immediately following the opening fence.
-- Returns a VTaggedString with the tag and content.
--
-- === Examples
--
-- >>> parse parseTaggedFencedString "" "```md\\n# Title\\n```"
-- Right (VTaggedString "md" "# Title")
--
-- >>> parse parseTaggedFencedString "" "```json\\n{}\\n```"
-- Right (VTaggedString "json" "{}")
--
-- === Errors
--
-- Fails if:
--
-- * No valid symbol follows opening fence
-- * Tag is not followed by newline
-- * Closing fence is missing
parseTaggedFencedString :: Parser Value
parseTaggedFencedString = do
  -- Match opening fence: ```
  void $ string "```"
  -- Parse the tag (must be a valid symbol)
  tag <- parseSymbol
  -- Require newline after tag
  void $ char '\n'
  -- Parse content until closing fence
  content <- parseFencedContent
  return $ V.VTaggedString (symbolToString tag) content
  where
    symbolToString (Symbol s) = s

parseTaggedString :: Parser Value
parseTaggedString = do
  tag <- parseSymbol
  void $ char '`'
  content <- manyTill (escapedChar '`') (char '`')
  return $ V.VTaggedString (quoteSymbol tag) content
  where
    quoteSymbol (Symbol s) = s

parseArray :: Parser Value
parseArray = do
  void $ char '['
  optionalSpace
  values <- sepBy (try parseScalarValue) (try (optionalSpaceWithNewlines >> char ',') >> optionalSpaceWithNewlines)
  optionalSpace
  void $ char ']'
  return $ V.VArray values

parseMap :: Parser Value
parseMap = do
  void $ char '{'
  optionalSpaceWithNewlines
  pairs <- sepBy (try parseMapping) (try (optionalSpaceWithNewlines >> char ',') >> optionalSpaceWithNewlines)
  optionalSpaceWithNewlines
  void $ char '}'
  return $ V.VMap (Map.fromList pairs)
  where
    parseMapping = do
      key <- parseIdentifier
      void $ optionalSpaceWithNewlines >> char ':' >> optionalSpaceWithNewlines
      value <- parseScalarValue
      optionalSpaceWithNewlines
      return (identifierToString key, value)

parseRange :: Parser Value
parseRange = do
  startsWithDots <- lookAhead (optional (try (string "..")))
  lower <- if startsWithDots == Just ".."
    then return Nothing
    else do
      sign <- optional (char '-')
      intPart <- some digitChar
      hasDots <- lookAhead (optional (try (string "..")))
      if hasDots == Just ".."
        then do
          let num = read intPart :: Double
          let numWithSign = if sign == Just '-' then -num else num
          return (Just numWithSign)
        else fail "not a range"
  firstDot <- char '.'
  secondDot <- char '.'
  hasThirdDot <- optional (char '.')
  if hasThirdDot == Just '.'
    then do
      upper <- optional (try parseRangeDouble)
      return $ V.VRange (V.RangeValue lower upper)
    else do
      upper <- if lower == Nothing
        then optional (try parseRangeDouble)
        else Just <$> parseRangeDouble
      return $ V.VRange (V.RangeValue lower upper)
  where
    parseRangeDouble = do
      sign <- optional (char '-')
      intPart <- some digitChar
      fracPart <- optional (char '.' >> some digitChar)
      let numStr = intPart ++ maybe "" ('.' :) fracPart
      let num = read numStr :: Double
      return $ if sign == Just '-' then -num else num

parseMeasurement :: Parser Value
parseMeasurement = do
  sign <- optional (char '-')
  intPart <- some digitChar
  fracPart <- optional (char '.' >> some digitChar)
  unit <- some (satisfy (\c -> isAlpha c || c == '%'))
  let numStr = intPart ++ maybe "" ('.' :) fracPart
  let num = read numStr :: Double
  let value = if sign == Just '-' then -num else num
  return $ V.VMeasurement unit value

parseScalarValue :: Parser Value
parseScalarValue = 
  try parseRange <|>
  try parseMeasurement <|>
  try (V.VDecimal <$> parseDecimal) <|>
  try (V.VInteger <$> parseInteger) <|>
  try (V.VBoolean <$> parseBoolean) <|>
  try parseFencedString <|>        -- Plain codefence (US1)
  try parseTaggedFencedString <|>  -- Tagged codefence (US2)
  try parseTaggedString <|>        -- Inline tagged string
  try (V.VString <$> parseString) <|>
  (V.VSymbol . quoteSymbol <$> parseSymbol)
  where
    quoteSymbol (Symbol s) = s

parseValue :: Parser Value
parseValue = 
  try parseRange <|>
  try parseMeasurement <|>
  try (V.VDecimal <$> parseDecimal) <|>
  try (V.VInteger <$> parseInteger) <|>
  try (V.VBoolean <$> parseBoolean) <|>
  try parseFencedString <|>        -- Plain codefence (US1)
  try parseTaggedFencedString <|>  -- Tagged codefence (US2)
  try parseTaggedString <|>        -- Inline tagged string
  try (V.VString <$> parseString) <|>
  try parseArray <|>
  try parseMap <|>
  (V.VSymbol . quoteSymbol <$> parseSymbol)
  where
    quoteSymbol (Symbol s) = s

identifierToString :: Identifier -> String
identifierToString (IdentSymbol (Symbol s)) = s
identifierToString (IdentString s) = s
identifierToString (IdentInteger i) = show i

parseIdentifier :: Parser Identifier
parseIdentifier = 
  try (IdentSymbol <$> parseSymbol) <|>
  try (IdentString <$> parseString) <|>
  try (IdentInteger <$> parseInteger)

parseLabelSeparator :: Parser ()
parseLabelSeparator = (try (string "::" >> return ()) <|> (char ':' >> return ()))

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

parsePropertyRecord :: Parser (Map String Value)
parsePropertyRecord = do
  void $ char '{'
  optionalSpaceWithNewlines
  pairs <- sepBy (try parseProperty) (try (optionalSpaceWithNewlines >> char ',') >> optionalSpaceWithNewlines)
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
      optionalSpaceWithNewlines
      return (identifierToString key, value)

-- ... [CST Specific Parsers] ...

parseAnnotation :: Parser Annotation
parseAnnotation = do
  void $ char '@'
  key <- parseSymbol
  void $ char '('
  value <- parseValue
  void $ char ')'
  optionalSpace
  return $ Annotation key value

parseAnnotations :: Parser [Annotation]
parseAnnotations = many (try parseAnnotation)

parseSubjectData :: Parser SubjectData
parseSubjectData = do
  nextChar <- lookAhead (satisfy (const True))
  case nextChar of
    '{' -> do
      props <- try parsePropertyRecord
      return $ SubjectData Nothing Set.empty props
    ':' -> do
      lbls <- parseLabels
      optionalSpace
      props <- optional (try parsePropertyRecord)
      return $ SubjectData Nothing lbls (maybe Map.empty id props)
    _ -> do
      ident <- parseIdentifier
      optionalSpace
      hasLabels <- lookAhead (optional (try (char ':')))
      lbls <- if hasLabels == Just ':'
        then do
          labels <- parseLabels
          optionalSpace
          return labels
        else return Set.empty
      optionalSpace
      hasRecord <- lookAhead (optional (char '{'))
      props <- if hasRecord == Just '{'
        then parsePropertyRecord
        else return Map.empty
      return $ SubjectData (Just ident) lbls props

parseNode :: Parser Node
parseNode = do
  void $ char '('
  optionalSpace
  data' <- try (Just <$> parseSubjectData) <|> return Nothing
  optionalSpace
  void $ char ')'
  return $ Node data'

parseReference :: Parser PatternElement
parseReference = do
  ident <- parseIdentifier
  optionalSpace
  return $ PEReference ident

parseRelationshipKind :: Parser String
parseRelationshipKind = 
  try (string "<==>") <|>
  try (string "<-->") <|>
  try (string "<~~>") <|>
  try (string "<--") <|>
  try (string "-->") <|>
  try (string "<==>") <|>
  try (string "==>") <|>
  try (string "<==") <|>
  try (string "<~~>") <|>
  try (string "~~>") <|>
  try (string "<~~") <|>
  try (string "==") <|>
  try (string "~~") <|>
  string "--"

-- | Parses the arrow part (including potential attributes)
parseArrow :: Parser (String, Maybe SubjectData)
parseArrow = 
  try parseInterruptedArrow <|>
  parseSimpleArrow
  where
    parseSimpleArrow = do
      kind <- parseRelationshipKind
      return (kind, Nothing)
      
    parseInterruptedArrow = do
      -- Simplified for now, capturing the whole interrupted arrow logic is complex
      -- But we need to capture attributes inside.
      -- (This mirrors existing logic but adapted for CST)
       prefix <- choice
         [ try (string "<-")
         , try (string "<=")
         , try (string "<~")
         , try (string "-")
         , try (string "=")
         , try (string "~")
         ]
       void $ char '['
       optionalSpace
       -- Annotations are no longer allowed in relationships
       optionalSpace
       data' <- optional parseSubjectData
       optionalSpace
       void $ char ']'
       suffix <- choice
         [ try (string "->")
         , try (string "=>")
         , try (string "~>")
         , try (string "-")
         , try (string "=")
         , try (string "~")
         ]
       return (prefix ++ "..." ++ suffix, data')

parsePath :: Parser Path
parsePath = do
  start <- parseNode
  optionalSpace
  segments <- many (try parsePathSegment)
  return $ Path start segments

parsePathSegment :: Parser PathSegment
parsePathSegment = do
  (arrow, data') <- parseArrow
  optionalSpace
  next <- parseNode
  optionalSpace
  return $ PathSegment (Relationship arrow data') next

parseSubPatternElement :: Parser PatternElement
parseSubPatternElement = try (PESubjectPattern <$> parseSubjectPattern) <|> try (PEPath <$> parsePath) <|> try parseReference

parseSubjectPattern :: Parser SubjectPattern
parseSubjectPattern = do
  void $ char '['
  optionalSpace
  data' <- optional parseSubjectData
  optionalSpace
  nested <- optional (do
    void $ char '|'
    optionalSpaceWithNewlines
    elements <- sepBy1 (try parseSubPatternElement) (try (optionalSpaceWithNewlines >> char ',') >> optionalSpaceWithNewlines)
    optionalSpace
    return elements)
  optionalSpaceWithNewlines
  void $ char ']'
  return $ SubjectPattern data' (maybe [] id nested)

parsePatternElement :: Parser PatternElement
parsePatternElement = try (PESubjectPattern <$> parseSubjectPattern) <|> (PEPath <$> parsePath)

parseAnnotatedPattern :: Parser AnnotatedPattern
parseAnnotatedPattern = do
  optionalSpace
  anns <- parseAnnotations
  optionalSpace
  -- Strict Mode: AnnotatedPattern contains exactly ONE PatternElement.
  -- Comma-separated sequences are only allowed inside SubjectPattern [...].
  element <- parsePatternElement
  return $ AnnotatedPattern anns [element]

parseGram :: Parser Gram
parseGram = do
  optionalSpace
  rootRecord <- optional (try parsePropertyRecord)
  optionalSpace
  
  firstPatterns <- if rootRecord == Nothing
    then (:[]) <$> parseAnnotatedPattern
    else optional (try parseAnnotatedPattern) >>= \p -> return $ maybe [] (:[]) p
    
  additionalPatterns <- many (try (do
    optionalSpaceWithNewlines
    -- Commas are NOT allowed as separators at the top level in strict gram
    -- void $ optional (char ',')
    nextChar <- lookAhead (satisfy (const True))
    if nextChar == '(' || nextChar == '[' || nextChar == '@'
      then parseAnnotatedPattern
      else fail "no pattern"))
      
  optionalSpaceWithNewlines
  eof
  
  return $ Gram rootRecord (firstPatterns ++ additionalPatterns)

-- | Main entry point transforming CST to Pattern
fromGram :: String -> Either ParseError (Core.Pattern CoreSub.Subject)
fromGram input = do
  let stripped = stripComments input
  case parse parseGram "gram" stripped of
    Left err -> Left (convertError err)
    Right cst -> Right (Transform.transformGram cst)
