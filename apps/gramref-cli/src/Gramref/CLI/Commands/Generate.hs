{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Gramref.CLI.Commands.Generate
  ( GenerateOptions(..)
  , generateOptions
  , runGenerate
  , TestSuite(..)
  , generateTestSuite
  ) where

import Options.Applicative
import Gramref.CLI.Types (OutputFormat(..), OutputOptions(..), outputOptionsParser, enforceDeterministicCanonical, canonical)
import qualified Gramref.CLI.Output as Output
import qualified Gramref.CLI.JSON as JSON
import qualified Pattern.Core as Pattern
import qualified Subject.Core as Subject
import qualified Subject.Value as SubjectValue
import qualified Gram.Parse as Gram
import qualified Gram.Serialize as Gram
import System.Exit (ExitCode(..))
import System.Random (StdGen, mkStdGen, randomR, split, random)
import Data.Aeson (ToJSON(..), object, (.=), Value)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Key (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.IO as TIO
import qualified Data.Set as Set
import qualified Data.Map as Map
import Text.Printf (printf)
import GHC.Generics (Generic)
import Data.Maybe (listToMaybe)

data GeneratorType
  = GenPattern
  | GenGraph
  | GenSuite
  | GenProperty
  deriving (Show, Eq)

data GenerateOptions = GenerateOptions
  { generateType :: GeneratorType
  , generateCount :: Int
  , generateSeed :: Maybe Int
  , generateComplexity :: String
  , generateFormat :: OutputFormat
  , generateOutputOptions :: OutputOptions
  } deriving (Show)

generateOptions :: Parser GenerateOptions
generateOptions = GenerateOptions
  <$> generatorTypeOption
  <*> countOption
  <*> seedOption
  <*> complexityOption
  <*> formatOption
  <*> outputOptionsParser

generatorTypeOption :: Parser GeneratorType
generatorTypeOption = option (maybeReader parseGeneratorType)
  ( long "type"
  <> short 't'
  <> metavar "TYPE"
  <> value GenPattern
  <> help "Generator type: pattern, graph, suite, or property (default: pattern)"
  )

parseGeneratorType :: String -> Maybe GeneratorType
parseGeneratorType "pattern" = Just GenPattern
parseGeneratorType "graph" = Just GenGraph
parseGeneratorType "suite" = Just GenSuite
parseGeneratorType "property" = Just GenProperty
parseGeneratorType _ = Nothing

countOption :: Parser Int
countOption = option auto
  ( long "count"
  <> short 'c'
  <> metavar "N"
  <> value 1
  <> help "Number of items to generate (default: 1)"
  )

seedOption :: Parser (Maybe Int)
seedOption = optional $ option auto
  ( long "seed"
  <> short 's'
  <> metavar "SEED"
  <> help "Random seed for deterministic generation"
  )

complexityOption :: Parser String
complexityOption = strOption
  ( long "complexity"
  <> metavar "LEVEL"
  <> value "basic"
  <> help "Complexity level: minimal, basic, standard, complex, adversarial (default: basic)"
  )

formatOption :: Parser OutputFormat
formatOption = option (maybeReader parseFormatStr)
  ( long "format"
  <> short 'f'
  <> metavar "FORMAT"
  <> value FormatJSON
  <> help "Output format: json, gram, or debug (default: json)"
  )

parseFormatStr :: String -> Maybe OutputFormat
parseFormatStr "json" = Just FormatJSON
parseFormatStr "gram" = Just FormatGram
parseFormatStr "debug" = Just FormatDebug
parseFormatStr _ = Nothing

runGenerate :: GenerateOptions -> IO ExitCode
runGenerate opts = do
  let seed = maybe 42 id (generateSeed opts)
  let gen = mkStdGen seed
  let outputOpts = enforceDeterministicCanonical (generateOutputOptions opts)
  
  case generateType opts of
    GenPattern -> do
      let patterns = take (generateCount opts) $ generatePatterns gen (generateComplexity opts)
      Output.formatOutput (generateFormat opts) outputOpts patterns
      return ExitSuccess
    GenSuite -> do
      let testSuite = generateTestSuite (generateCount opts) gen (generateComplexity opts)
      let jsonVal = toJSON testSuite
      let jsonVal' = if canonical outputOpts then JSON.canonicalizeJSON jsonVal else jsonVal
      let jsonBytes = encodePretty jsonVal'
      TIO.putStrLn $ TE.decodeUtf8 $ BSL.toStrict jsonBytes
      return ExitSuccess
    _ -> do
      Output.formatError (generateFormat opts) outputOpts "Generator type not yet implemented"
      return (ExitFailure 3)

-- Test Suite Data Types

data TestSuite = TestSuite
  { testSuiteVersion :: T.Text
  , testSuiteTestCases :: [TestCase]
  } deriving (Generic, Show)

instance ToJSON TestSuite where
  toJSON (TestSuite version testCases) = object
    [ "version" .= version
    , "test_cases" .= testCases
    ]

data TestCase = TestCase
  { testCaseName :: T.Text
  , testCaseDescription :: T.Text
  , testCaseInput :: TestInput
  , testCaseExpected :: TestExpected
  , testCaseOperations :: Maybe [TestOperation]
  } deriving (Generic, Show)

instance ToJSON TestCase where
  toJSON (TestCase name desc input expected ops) = object
    [ "name" .= name
    , "description" .= desc
    , "input" .= input
    , "expected" .= expected
    , "operations" .= ops
    ]

data TestInput = TestInput
  { testInputType :: T.Text
  , testInputValue :: T.Text
  } deriving (Generic, Show)

instance ToJSON TestInput where
  toJSON (TestInput typ val) = object
    [ "type" .= typ
    , "value" .= val
    ]

data TestExpected = TestExpected
  { testExpectedType :: T.Text
  , testExpectedValue :: Value
  } deriving (Generic, Show)

instance ToJSON TestExpected where
  toJSON (TestExpected typ val) = object
    [ "type" .= typ
    , "value" .= val
    ]

data TestOperation = TestOperation
  { testOperationOp :: T.Text
  , testOperationAgainst :: T.Text
  , testOperationExpectedBindings :: Maybe Value
  } deriving (Generic, Show)

instance ToJSON TestOperation where
  toJSON (TestOperation op against bindings) = object
    [ "op" .= op
    , "against" .= against
    , "expected_bindings" .= bindings
    ]

-- Test Suite Generation

generateTestSuite :: Int -> StdGen -> String -> TestSuite
generateTestSuite count gen complexity =
  let testCases = generateTestCases 1 count gen complexity
  in TestSuite "1.0" testCases

generateTestCases :: Int -> Int -> StdGen -> String -> [TestCase]
generateTestCases currentNum totalCount gen complexity
  | currentNum > totalCount = []
  | otherwise =
      let testCase = generateTestCase currentNum gen complexity
          (_, gen2) = split gen
      in testCase : generateTestCases (currentNum + 1) totalCount gen2 complexity

generateTestCase :: Int -> StdGen -> String -> TestCase
generateTestCase caseNum gen complexity =
  let name = T.pack $ "test_case_" ++ printf "%03d" caseNum
      description = T.pack $ "Generated test case " ++ show caseNum ++ " with " ++ complexity ++ " complexity"
      (_, gramNotation) = generatePatternForComplexity gen complexity
      input = TestInput "gram_notation" (T.pack gramNotation)
      -- Parse the gram notation and use the parsed result; on parse failure, record
      -- the error as expected so we don't mask round-trip bugs or produce inconsistent
      -- test data (expected pattern when the input doesn't parse).
      expected = case Gram.fromGram gramNotation of
        Right pats -> case pats of
          [pat] -> TestExpected "pattern" (patternToJSONValue pat)
          _ -> TestExpected "patternList" (toJSON pats)
        Left err -> TestExpected "parseError" (toJSON (show err))
      operations = Nothing  -- TODO: Add operations generation
  in TestCase name description input expected operations

-- Helper to convert pattern to JSON Value
patternToJSONValue :: Pattern.Pattern Subject.Subject -> Value
patternToJSONValue (Pattern.Pattern v es) = object
  [ "value" .= subjectToJSONValue v
  , "elements" .= map patternToJSONValue es
  ]

subjectToJSONValue :: Subject.Subject -> Value
subjectToJSONValue (Subject.Subject sym labels props) = object
  [ "symbol" .= symbolToJSONValue sym
  , "labels" .= toJSON (Set.toList labels)
  , "properties" .= propsToJSONValue props
  ]

symbolToJSONValue :: Subject.Symbol -> Value
symbolToJSONValue (Subject.Symbol s) = toJSON s

propsToJSONValue :: Subject.PropertyRecord -> Value
propsToJSONValue props = object $ map (\(k, v) -> fromString k .= valueToJSONValue v) (Map.toList props)

valueToJSONValue :: SubjectValue.Value -> Value
valueToJSONValue (SubjectValue.VInteger i) = toJSON i
valueToJSONValue (SubjectValue.VDecimal d) = toJSON d
valueToJSONValue (SubjectValue.VBoolean b) = toJSON b
valueToJSONValue (SubjectValue.VString s) = toJSON s
valueToJSONValue (SubjectValue.VSymbol s) = object ["type" .= ("symbol" :: T.Text), "value" .= s]
valueToJSONValue (SubjectValue.VTaggedString tag content) = object ["type" .= ("tagged" :: T.Text), "tag" .= tag, "content" .= content]
valueToJSONValue (SubjectValue.VArray vs) = toJSON (map valueToJSONValue vs)
valueToJSONValue (SubjectValue.VMap m) = toJSON (Map.map valueToJSONValue m)
valueToJSONValue (SubjectValue.VRange rv) = rangeValueToJSONValue rv
valueToJSONValue (SubjectValue.VMeasurement unit val) = object ["type" .= ("measurement" :: T.Text), "unit" .= unit, "value" .= val]

rangeValueToJSONValue :: SubjectValue.RangeValue -> Value
rangeValueToJSONValue (SubjectValue.RangeValue lower upper) = object
  [ "type" .= ("range" :: T.Text)
  , "lower" .= toJSON lower
  , "upper" .= toJSON upper
  ]

generatePatternForComplexity :: StdGen -> String -> (Pattern.Pattern Subject.Subject, String)
generatePatternForComplexity gen complexity =
  case complexity of
    "minimal" -> generateMinimalPattern gen
    "basic" -> generateBasicPattern gen
    "standard" -> generateStandardPattern gen
    "complex" -> generateComplexPattern gen
    "adversarial" -> generateAdversarialPattern gen
    _ -> generateBasicPattern gen  -- Default to basic

-- Helper functions for pattern generation

randomLabel :: StdGen -> (String, StdGen)
randomLabel gen =
  let labels = ["Person", "Node", "Entity", "Thing", "Item", "Object", "Resource"]
      (idx, gen') = randomR (0 :: Int, length labels - 1) gen
  in (labels !! idx, gen')

randomProperty :: StdGen -> ((String, SubjectValue.Value), StdGen)
randomProperty gen =
  let (keyIdx, gen1) = randomR (0 :: Int, 2 :: Int) gen
      keys = ["name", "id", "value"]
      key = keys !! keyIdx
      (valType, gen2) = randomR (0 :: Int, 2 :: Int) gen1
      (val, gen3) = case valType of
        0 -> let (n, g) = randomR (1 :: Int, 100 :: Int) gen2 in (SubjectValue.VInteger (toInteger n), g)
        1 -> let (n, g) = randomR (1 :: Int, 100 :: Int) gen2 in (SubjectValue.VString ("val" ++ show n), g)
        _ -> (SubjectValue.VBoolean True, gen2)
  in ((key, val), gen3)

generateMinimalPattern :: StdGen -> (Pattern.Pattern Subject.Subject, String)
generateMinimalPattern gen =
  let (n, _) = randomR (1 :: Int, 5 :: Int) gen
      symbol = Subject.Symbol ("n" ++ show n)
      subject = Subject.Subject symbol mempty mempty
      pattern = Pattern.Pattern subject []
      gramNotation = "(n" ++ show n ++ ")"
  in (pattern, gramNotation)

generateBasicPattern :: StdGen -> (Pattern.Pattern Subject.Subject, String)
generateBasicPattern gen =
  let (pattern, _, _) = generateBasicPatternWithGen gen
      gramNotation = Gram.serializePattern pattern
  in (pattern, gramNotation)

generateBasicPatternWithGen :: StdGen -> (Pattern.Pattern Subject.Subject, String, StdGen)
generateBasicPatternWithGen gen =
  let (n, gen1) = randomR (1 :: Int, 10 :: Int) gen
      (hasLabel, gen2) = random gen1
      (labels, gen3) = if hasLabel then
          let (label, g) = randomLabel gen2 in (Set.fromList [label], g)
        else (mempty, gen2)
      symbol = Subject.Symbol ("node" ++ show n)
      subject = Subject.Subject symbol labels mempty
      pattern = Pattern.Pattern subject []
      labelStr = case listToMaybe (Set.toList labels) of
        Just label -> ":" ++ label
        Nothing -> ""
      gramNotation = "(node" ++ show n ++ labelStr ++ ")"
  in (pattern, gramNotation, gen3)

generateStandardPattern :: StdGen -> (Pattern.Pattern Subject.Subject, String)
generateStandardPattern gen =
  let (pattern, _, _) = generateStandardPatternWithGen gen
      gramNotation = Gram.serializePattern pattern
  in (pattern, gramNotation)

generateStandardPatternWithGen :: StdGen -> (Pattern.Pattern Subject.Subject, String, StdGen)
generateStandardPatternWithGen gen =
  let (n, gen1) = randomR (1 :: Int, 20 :: Int) gen
      (hasLabel, gen2) = random gen1
      (labels, gen3) = if hasLabel then
          let (label, g) = randomLabel gen2 in (Set.fromList [label], g)
        else (mempty, gen2)
      (hasProp, gen4) = random gen3
      (props, gen5) = if hasProp then
          let ((k, v), g) = randomProperty gen4
          in (Map.fromList [(k, v)], g)
        else (mempty, gen4)
      symbol = Subject.Symbol ("node" ++ show n)
      subject = Subject.Subject symbol labels props
      pattern = Pattern.Pattern subject []
      labelStr = case listToMaybe (Set.toList labels) of
        Just label -> ":" ++ label
        Nothing -> ""
      propStr = if Map.null props then "" else
          case listToMaybe (Map.toList props) of
            Just (k, v) ->
              let valStr = case v of
                    SubjectValue.VInteger i -> show i
                    SubjectValue.VString s -> "\"" ++ s ++ "\""
                    SubjectValue.VBoolean b -> if b then "true" else "false"
                    _ -> "null"
              in " {" ++ k ++ ":" ++ valStr ++ "}"
            Nothing -> ""
      gramNotation = "(node" ++ show n ++ labelStr ++ propStr ++ ")"
  in (pattern, gramNotation, gen5)

generateComplexPattern :: StdGen -> (Pattern.Pattern Subject.Subject, String)
generateComplexPattern gen =
  let (n, gen1) = randomR (1 :: Int, 50 :: Int) gen
      (hasLabel, gen2) = random gen1
      (labels, gen3) = if hasLabel then
          let (label, g) = randomLabel gen2 in (Set.fromList [label], g)
        else (mempty, gen2)
      (propCount, gen4) = randomR (0 :: Int, 2 :: Int) gen3
      (props, gen5) = if propCount > 0 then
          foldl (\(m, g) _ ->
            let ((k, v), g') = randomProperty g
            in (Map.insert k v m, g')) (mempty, gen4) [1..propCount]
        else (mempty, gen4)
      (hasElements, gen6) = random gen5
      (elements, _) = if hasElements then
          let (elemCount, g1) = randomR (1 :: Int, 2 :: Int) gen6
              (elems, g2) = foldl (\(es, g) _ ->
                let (p, _, g') = generateBasicPatternWithGen g
                in (es ++ [p], g')) ([], g1) [1..elemCount]
          in (elems, g2)
        else ([], gen6)
      symbol = Subject.Symbol ("node" ++ show n)
      subject = Subject.Subject symbol labels props
      pattern = Pattern.Pattern subject elements
      gramNotation = Gram.serializePattern pattern
  in (pattern, gramNotation)

generateAdversarialPattern :: StdGen -> (Pattern.Pattern Subject.Subject, String)
generateAdversarialPattern gen =
  let (n, gen1) = randomR (1 :: Int, 100 :: Int) gen
      (hasLabel, gen2) = random gen1
      (labels, gen3) = if hasLabel then
          let (label, g) = randomLabel gen2 in (Set.fromList [label], g)
        else (mempty, gen2)
      (propCount, gen4) = randomR (0 :: Int, 3 :: Int) gen3
      (props, gen5) = if propCount > 0 then
          foldl (\(m, g) _ ->
            let ((k, v), g') = randomProperty g
            in (Map.insert k v m, g')) (mempty, gen4) [1..propCount]
        else (mempty, gen4)
      (hasElements, gen6) = random gen5
      (elements, _) = if hasElements then
          let (elemCount, g1) = randomR (1 :: Int, 3 :: Int) gen6
              (elems, g2) = foldl (\(es, g) _ ->
                let (p, _, g') = generateStandardPatternWithGen g
                in (es ++ [p], g')) ([], g1) [1..elemCount]
          in (elems, g2)
        else ([], gen6)
      symbol = Subject.Symbol ("node" ++ show n)
      subject = Subject.Subject symbol labels props
      pattern = Pattern.Pattern subject elements
      gramNotation = Gram.serializePattern pattern
  in (pattern, gramNotation)

-- Pattern generation (existing)

generatePatterns :: StdGen -> String -> [Pattern.Pattern Subject.Subject]
generatePatterns gen complexity = 
  let (_, gen') = randomR (1 :: Int, 5 :: Int) gen
      subject = Subject.Subject (Subject.Symbol "gen") mempty mempty
  in Pattern.Pattern subject [] : generatePatterns gen' complexity

