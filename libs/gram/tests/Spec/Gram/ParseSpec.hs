-- | Unit tests for Gram.Parse module.
module Spec.Gram.ParseSpec where

import Test.Hspec
import Gram.Parse (fromGram, ParseError(..))
import Pattern.Core (Pattern(..))
import Subject.Core (Subject(..), Symbol(..))
import Data.Set (Set)
import qualified Data.Set as Set

spec :: Spec
spec = do
  describe "Gram.Parse" $ do
    
    describe "fromGram" $ do
      
      it "is a placeholder (not yet implemented)" $ do
        -- This will fail until fromGram is implemented
        fromGram "(n:Person)" `shouldBe` Left (ParseError "fromGram: Not yet implemented")

