{-# LANGUAGE OverloadedStrings #-}

module PrettyPrintingSpec where

import Control.Monad
import Distribution.Nixpkgs.Haskell.Packages.PrettyPrinting
import Test.Hspec

-- test stuff
spec :: Spec
spec = specify "test" $
  when False $ print (overrides "body")
