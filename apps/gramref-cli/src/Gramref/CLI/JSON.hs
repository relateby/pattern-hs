{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Gramref.CLI.JSON
  ( patternsToJSON
  , errorToJSON
  , canonicalizeJSON
  , Meta(..)
  , PatternResult(..)
  , Diagnostics(..)
  , ErrorResponse(..)
  ) where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (utcToZonedTime, utc)
import System.IO.Unsafe (unsafePerformIO)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Lazy as BSL
import qualified Pattern.Core as Pattern
import qualified Subject.Core as Subject
import qualified Gramref.CLI.Types as Types
import qualified Gram.JSON as GramJSON
import GHC.Generics (Generic)

data Meta = Meta
  { metaVersion :: T.Text
  , metaCommand :: T.Text
  , metaTimestamp :: T.Text
  , metaHash :: T.Text
  } deriving (Generic, Show)

instance ToJSON Meta where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 4 }

data Diagnostics = Diagnostics
  { diagnosticsTimeMs :: Double
  , diagnosticsMemoryBytes :: Integer
  } deriving (Generic, Show)

instance ToJSON Diagnostics where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 12 }

data PatternResult = PatternResult
  { resultType :: T.Text
  , resultValue :: Value
  } deriving (Generic, Show)

instance ToJSON PatternResult where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 6 }

data SuccessResponse = SuccessResponse
  { successMeta :: Meta
  , successResult :: PatternResult
  , successDiagnostics :: Maybe Diagnostics
  } deriving (Generic, Show)

instance ToJSON SuccessResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 7 }

data ErrorLocation = ErrorLocation
  { errorLocationFile :: Maybe T.Text
  , errorLocationLine :: Maybe Int
  , errorLocationColumn :: Maybe Int
  , errorLocationContext :: Maybe T.Text
  } deriving (Generic, Show)

instance ToJSON ErrorLocation where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 14 }

data ErrorResponse = ErrorResponse
  { errorType :: T.Text
  , errorMessage :: T.Text
  , errorLocation :: Maybe ErrorLocation
  , errorSuggestion :: Maybe T.Text
  } deriving (Generic, Show)

instance ToJSON ErrorResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 5 }

data ErrorWrapper = ErrorWrapper
  { errorError :: ErrorResponse
  } deriving (Generic, Show)

instance ToJSON ErrorWrapper where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 5 }

-- Patterns serialization to JSON
patternsToJSON :: Types.OutputOptions -> [Pattern.Pattern Subject.Subject] -> T.Text
patternsToJSON opts pats = unsafePerformIO $ do
  let opts' = Types.enforceDeterministicCanonical opts
  let patternsVal = toJSON pats  -- Use the ToJSON instance from Gram.JSON
  
  -- Handle value-only output
  if Types.valueOnly opts' 
    then do
      let jsonVal = if Types.canonical opts' then GramJSON.canonicalizeJSON patternsVal else patternsVal
      -- Use encode for canonical output (encodePretty only affects top-level keys)
      let jsonBytes = encode jsonVal
      return $ TE.decodeUtf8 $ BSL.toStrict jsonBytes
    else do
      -- Generate metadata
      timestamp <- if Types.deterministic opts'
        then return "1970-01-01T00:00:00+0000"
        else do
          now <- getCurrentTime
          return $ T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%z" (utcToZonedTime utc now)
      let fixedHash = "0000000000000000000000000000000000000000000000000000000000000000"
      let response = SuccessResponse
            { successMeta = Meta
                { metaVersion = "0.1.0"
                , metaCommand = "parse"
                , metaTimestamp = timestamp
                , metaHash = ""  -- Will compute after encoding
                }
            , successResult = PatternResult
                { resultType = "PatternList"
                , resultValue = patternsVal
                }
            , successDiagnostics = Nothing
            }
      -- Convert to JSON Value first, then canonicalize if needed before computing hash
      let responseJson = toJSON response
      let jsonForHash = if Types.canonical opts'
            then GramJSON.canonicalizeJSON responseJson
            else responseJson
      let jsonBytesForHash = encodePretty jsonForHash
      let hash' = if Types.deterministic opts'
            then fixedHash
            else T.pack $ show $ SHA256.hash $ BSL.toStrict jsonBytesForHash
      let responseWithHash = response { successMeta = (successMeta response) { metaHash = hash' } }
      -- Now canonicalize the final output (with hash included) if needed
      let finalJson = if Types.canonical opts' 
            then GramJSON.canonicalizeJSON $ toJSON responseWithHash
            else toJSON responseWithHash
      let finalJsonBytes = encodePretty finalJson
      return $ TE.decodeUtf8 $ BSL.toStrict finalJsonBytes

errorToJSON :: Types.OutputOptions -> String -> T.Text
errorToJSON opts msg = 
  let opts' = Types.enforceDeterministicCanonical opts
      errorResp = ErrorResponse
        { errorType = "ParseError"
        , errorMessage = T.pack msg
        , errorLocation = Nothing
        , errorSuggestion = Nothing
        }
      jsonVal = if Types.valueOnly opts'
        then toJSON errorResp
        else toJSON $ ErrorWrapper { errorError = errorResp }
      jsonVal' = if Types.canonical opts' then GramJSON.canonicalizeJSON jsonVal else jsonVal
      jsonBytes = encodePretty jsonVal'
  in TE.decodeUtf8 $ BSL.toStrict jsonBytes

-- | Re-export canonicalizeJSON from Gram.JSON for backward compatibility
canonicalizeJSON :: Value -> Value
canonicalizeJSON = GramJSON.canonicalizeJSON

