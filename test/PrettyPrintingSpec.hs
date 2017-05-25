module PrettyPrintingSpec where

import Distribution.Nixpkgs.Haskell.Packages.PrettyPrinting
import Test.Hspec

spec :: Spec
spec = specify "test" $
  print (overrides "body")
