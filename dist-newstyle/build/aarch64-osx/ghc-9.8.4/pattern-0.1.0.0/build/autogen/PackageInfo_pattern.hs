{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_pattern (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "pattern"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "A generalized representation of graph elements using category theory"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
