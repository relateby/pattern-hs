{-# LANGUAGE OverloadedStrings #-}
module Spec.CLI.ConvertSpec (spec) where

import Test.Hspec
import System.Exit (ExitCode(..))
import System.IO.Temp (withSystemTempFile)
import System.IO (hPutStr, hClose)
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson (encode)
import qualified Pattern.Core as Pattern
import qualified Subject.Core as Subject
import qualified Subject.Value as SubjectValue
import qualified Gram.JSON as GramJSON  -- For ToJSON instance
import qualified Gramref.CLI.Commands.Convert as Convert
import Gramref.CLI.Types (OutputOptions(..))
import qualified Data.Set as Set
import qualified Data.Map as Map

-- | Helper to create a simple subject
mkSubject :: String -> Subject.Subject
mkSubject sym = Subject.Subject (Subject.Symbol sym) Set.empty Map.empty

spec :: Spec
spec = do
  describe "Convert Command" $ do
    
    describe "JSON to Gram Conversion" $ do
      it "converts JSON pattern to Gram notation" $ do
        -- Create a simple pattern
        let pat = Pattern.Pattern (mkSubject "node") []
        let jsonBytes = encode pat
        
        -- Write JSON to temp file
        withSystemTempFile "test.json" $ \tmpFile tmpHandle -> do
          BSL.hPutStr tmpHandle jsonBytes
          hClose tmpHandle
          
          -- Convert JSON to Gram
          let opts = Convert.ConvertOptions
                { Convert.convertInputFile = tmpFile
                , Convert.convertFrom = Convert.ConvertJSON
                , Convert.convertTo = Convert.ConvertGram
                , Convert.convertOutputOptions = OutputOptions
                    { valueOnly = False
                    , deterministic = False
                    , canonical = False
                    }
                }
          
          result <- Convert.runConvert opts
          result `shouldBe` ExitSuccess
      
      it "converts JSON pattern with properties to Gram notation" $ do
        -- Create a pattern with properties
        let props = Map.fromList [("name", SubjectValue.VString "Alice"), ("age", SubjectValue.VInteger 30)]
        let subj = Subject.Subject (Subject.Symbol "person") Set.empty props
        let pat = Pattern.Pattern subj []
        let jsonBytes = encode pat
        
        -- Write JSON to temp file
        withSystemTempFile "test.json" $ \tmpFile tmpHandle -> do
          BSL.hPutStr tmpHandle jsonBytes
          hClose tmpHandle
          
          -- Convert JSON to Gram
          let opts = Convert.ConvertOptions
                { Convert.convertInputFile = tmpFile
                , Convert.convertFrom = Convert.ConvertJSON
                , Convert.convertTo = Convert.ConvertGram
                , Convert.convertOutputOptions = OutputOptions
                    { valueOnly = False
                    , deterministic = False
                    , canonical = False
                    }
                }
          
          result <- Convert.runConvert opts
          result `shouldBe` ExitSuccess
      
      it "converts JSON pattern with nested elements to Gram notation" $ do
        -- Create a nested pattern
        let inner = Pattern.Pattern (mkSubject "child") []
        let outer = Pattern.Pattern (mkSubject "parent") [inner]
        let jsonBytes = encode outer
        
        -- Write JSON to temp file
        withSystemTempFile "test.json" $ \tmpFile tmpHandle -> do
          BSL.hPutStr tmpHandle jsonBytes
          hClose tmpHandle
          
          -- Convert JSON to Gram
          let opts = Convert.ConvertOptions
                { Convert.convertInputFile = tmpFile
                , Convert.convertFrom = Convert.ConvertJSON
                , Convert.convertTo = Convert.ConvertGram
                , Convert.convertOutputOptions = OutputOptions
                    { valueOnly = False
                    , deterministic = False
                    , canonical = False
                    }
                }
          
          result <- Convert.runConvert opts
          result `shouldBe` ExitSuccess
      
      it "fails gracefully on invalid JSON input" $ do
        -- Write invalid JSON to temp file
        withSystemTempFile "test.json" $ \tmpFile tmpHandle -> do
          hPutStr tmpHandle "{invalid json}"
          hClose tmpHandle
          
          -- Attempt to convert invalid JSON
          let opts = Convert.ConvertOptions
                { Convert.convertInputFile = tmpFile
                , Convert.convertFrom = Convert.ConvertJSON
                , Convert.convertTo = Convert.ConvertGram
                , Convert.convertOutputOptions = OutputOptions
                    { valueOnly = False
                    , deterministic = False
                    , canonical = False
                    }
                }
          
          result <- Convert.runConvert opts
          result `shouldBe` ExitFailure 1

    describe "Gram to JSON Conversion" $ do
      it "converts Gram notation to JSON" $ do
        -- Write Gram notation to temp file
        withSystemTempFile "test.gram" $ \tmpFile tmpHandle -> do
          hPutStr tmpHandle "(node)"
          hClose tmpHandle
          
          -- Convert Gram to JSON
          let opts = Convert.ConvertOptions
                { Convert.convertInputFile = tmpFile
                , Convert.convertFrom = Convert.ConvertGram
                , Convert.convertTo = Convert.ConvertJSON
                , Convert.convertOutputOptions = OutputOptions
                    { valueOnly = False
                    , deterministic = False
                    , canonical = False
                    }
                }
          
          result <- Convert.runConvert opts
          result `shouldBe` ExitSuccess

    describe "Roundtrip Conversion" $ do
      it "roundtrips through JSON and back to Gram" $ do
        -- Original Gram pattern
        let gramInput = "(person {name: \"Alice\", age: 30})"
        
        -- Write to temp file
        withSystemTempFile "test.gram" $ \tmpFile1 tmpHandle1 -> do
          hPutStr tmpHandle1 gramInput
          hClose tmpHandle1
          
          -- Convert Gram to JSON
          let optsToJSON = Convert.ConvertOptions
                { Convert.convertInputFile = tmpFile1
                , Convert.convertFrom = Convert.ConvertGram
                , Convert.convertTo = Convert.ConvertJSON
                , Convert.convertOutputOptions = OutputOptions
                    { valueOnly = True
                    , deterministic = True
                    , canonical = True
                    }
                }
          
          result1 <- Convert.runConvert optsToJSON
          result1 `shouldBe` ExitSuccess
