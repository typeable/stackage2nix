{-# LANGUAGE OverloadedStrings #-}

module Language.Nix.FilePathSpec where

import           Data.String
import           Distribution.Text
import qualified Language.Nix.FilePath as Nix
import           Test.Hspec
import           Text.PrettyPrint as PP

displayPath :: String -> Doc -> SpecWith ()
displayPath path result = specify path
  $ disp (fromString path :: Nix.FilePath) `shouldBe` result

spec :: Spec
spec = describe "valid FilePath" $ do
  displayPath "foo" "./foo"
  displayPath "./foo" "./foo"
  displayPath "../foo" "./../foo"
  displayPath "/foo" "/foo"
  displayPath "." "./."
