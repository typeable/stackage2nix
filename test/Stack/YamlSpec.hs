{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Stack.YamlSpec where

import Data.ByteString
import Data.Text.Encoding as T
import Data.Yaml as Y
import Stack.Yaml
import Test.Hspec
import Text.Shakespeare.Text

configYaml :: ByteString
configYaml = T.encodeUtf8 [st|
resolver: lts-3.7
packages:
  - .
  - location: dir1/dir2
  - location: https://example.com/foo/bar/baz-0.0.2.tar.gz
    extra-dep: true
  - location:
      git: git@github.com:commercialhaskell/stack.git
      commit: 6a86ee32e5b869a877151f74064572225e1a0398
# Comment
extra-deps:
- acme-missiles-0.3
|]

config :: Config
config = Config
  { _cResolver = "lts-3.7"
  , _cPackages =
    [ Simple "."
    , LocationSimple (Location "dir1/dir2" Nothing)
    , LocationSimple (Location "https://example.com/foo/bar/baz-0.0.2.tar.gz" (Just True))
    , LocationGit $ Location
       (Git
        "git@github.com:commercialhaskell/stack.git"
        "6a86ee32e5b869a877151f74064572225e1a0398")
       Nothing]
  , _cExtraDeps =
    ["acme-missiles-0.3"]
  }

spec :: Spec
spec = describe "Config" $
  it "parsed" $
    (Y.decode configYaml :: Maybe Config) `shouldBe` Just config
