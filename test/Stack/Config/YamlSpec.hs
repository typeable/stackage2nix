{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Stack.Config.YamlSpec where

import Data.ByteString
import Data.Text.Encoding as T
import Data.Yaml as Y
import Stack.Config.Yaml
import Test.Hspec
import Text.Shakespeare.Text

configYamlMinimal :: ByteString
configYamlMinimal = T.encodeUtf8 [st|
resolver: lts-8.15
packages:
- '.'
|]

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
  - location:
      git: git@github.com:example/mega-repo
      commit: 6a86ee32e5b869a877151f74064572225e1a0000
    subdirs:
      - subdir1
      - subdir2
# Comment
extra-deps:
- acme-missiles-0.3
|]

-- | New yaml config support all package definitions in "extra-deps" section
configNewYaml :: ByteString
configNewYaml = T.encodeUtf8 [st|
resolver: lts-3.7
packages:
  - .
  - location: dir1/dir2
  - location: https://example.com/foo/bar/baz-0.0.2.tar.gz
    extra-dep: true
  - location:
      git: git@github.com:commercialhaskell/stack.git
      commit: 6a86ee32e5b869a877151f74064572225e1a0398
  - location:
      git: git@github.com:example/mega-repo
      commit: 6a86ee32e5b869a877151f74064572225e1a0000
    subdirs:
      - subdir1
      - subdir2
# Comment
extra-deps:
  - acme-missiles-0.3
  - acme-missiles-hash-0.4@@sha256:2ba66a092a32593880a87fb00f3213762d7bca65a687d45965778deb8694c5d1
  - acme-missiles-rev-0.5@@rev:0
  - git: git@github.com:commercialhaskell/stack.git
    commit: 6a86ee32e5b869a877151f74064572225e1a0398
  - git: git@github.com:example/mega-repo
    commit: 6a86ee32e5b869a877151f74064572225e1a0000
    subdirs:
      - subdir1
      - subdir2
|]


configMinimal :: Config
configMinimal = Config
  { _cResolver  = "lts-8.15"
  , _cPackages  = Just [ Simple "." ]
  , _cExtraDeps = Nothing
  }

config :: Config
config = Config
  { _cResolver = "lts-3.7"
  , _cPackages = Just
    [ Simple "."
    , LocationSimple (Location "dir1/dir2" Nothing Nothing)
    , LocationSimple (Location "https://example.com/foo/bar/baz-0.0.2.tar.gz" (Just True) Nothing)
    , LocationGit $ Location
       (Git
        "git@github.com:commercialhaskell/stack.git"
        "6a86ee32e5b869a877151f74064572225e1a0398")
       Nothing Nothing
    , LocationGit (Location
       (Git
        "git@github.com:example/mega-repo"
        "6a86ee32e5b869a877151f74064572225e1a0000")
       Nothing (Just ["subdir1", "subdir2"]))
    ]
  , _cExtraDeps = Just
    [Simple "acme-missiles-0.3"]
  }

configNew :: Config
configNew = Config
  { _cResolver = "lts-3.7"
  , _cPackages = Just
    [ Simple "."
    , LocationSimple (Location "dir1/dir2" Nothing Nothing)
    , LocationSimple (Location "https://example.com/foo/bar/baz-0.0.2.tar.gz" (Just True) Nothing)
    , LocationGit $ Location
       (Git
        "git@github.com:commercialhaskell/stack.git"
        "6a86ee32e5b869a877151f74064572225e1a0398")
       Nothing Nothing
    , LocationGit (Location
       (Git
        "git@github.com:example/mega-repo"
        "6a86ee32e5b869a877151f74064572225e1a0000")
       Nothing (Just ["subdir1", "subdir2"]))
    ]
  , _cExtraDeps = Just
    [ Simple "acme-missiles-0.3"
    , Simple "acme-missiles-hash-0.4@@sha256:2ba66a092a32593880a87fb00f3213762d7bca65a687d45965778deb8694c5d1"
    , Simple "acme-missiles-rev-0.5@@rev:0"
    , PNewGit $ NewGit
      "git@github.com:commercialhaskell/stack.git"
      "6a86ee32e5b869a877151f74064572225e1a0398"
      Nothing
    , PNewGit $ NewGit
      "git@github.com:example/mega-repo"
      "6a86ee32e5b869a877151f74064572225e1a0000"
      (Just ["subdir1", "subdir2"])
    ]
  }

spec :: Spec
spec = describe "Parse" $ do
  specify "config-minimal" $
    (Y.decode configYamlMinimal :: Maybe Config) `shouldBe` Just configMinimal
  specify "config-full" $
    (Y.decode configYaml :: Maybe Config) `shouldBe` Just config
  specify "config-new" $
    (Y.decode configNewYaml :: Maybe Config) `shouldBe` Just configNew
