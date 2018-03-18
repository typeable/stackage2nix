{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Stack.ConfigSpec (spec) where

import Data.ByteString
import Data.List.NonEmpty as NE
import Data.Text.Encoding as T
import Data.Yaml as Y
import Network.URI as URI
import Stack.Config
import Test.Hspec
import Text.Shakespeare.Text


spec :: Spec
spec = describe "StackConfig" $ do
  specify "minimal"
    $ checkYamlConfig minimalBs minimalConf
  specify "packageDefault"
    $ checkYamlConfig packageDefaultBs packageDefaultConf
  specify "packageSimpleList"
    $ checkYamlConfig packageSimpleListBs packageSimpleListConf
  specify "packageLocations"
    $ checkYamlConfig packageLocationsBs packageLocationsConf
  specify "packageExtraDeps"
    $ checkYamlConfig packageExtraDepsBs packageExtraDepsConf
  specify "packageMixed"
    $ checkYamlConfig packageMixedBs packageMixedConf
  specify "packageExtraDepsNew"
    $ checkYamlConfig packageExtraDepsNewBs packageExtraDepsNewConf

parseYamlConfig :: ByteString -> Maybe StackConfig
parseYamlConfig = fmap fromYamlConfig . Y.decode

checkYamlConfig :: ByteString -> StackConfig -> Expectation
checkYamlConfig bs sc = parseYamlConfig bs `shouldBe` Just sc

stUri :: String -> URI
stUri s =
  let Just uri = parseURI s
  in uri

minimalBs :: ByteString
minimalBs = T.encodeUtf8 [st|
resolver: lts-10.0
|]

minimalConf :: StackConfig
minimalConf = StackConfig
  { _scResolver  = StackResolver "lts-10.0"
  , _scPackages  = NE.fromList
    [ StackPackage (PlFilePath ".") False Nothing]
  }

packageDefaultBs :: ByteString
packageDefaultBs = T.encodeUtf8 [st|
resolver: lts-10.0
packages:
  - .
|]

packageDefaultConf :: StackConfig
packageDefaultConf = minimalConf

packageSimpleListBs :: ByteString
packageSimpleListBs = T.encodeUtf8 [st|
resolver: lts-10.0
packages:
  - client
  - server
|]

packageSimpleListConf :: StackConfig
packageSimpleListConf = StackConfig
  { _scResolver = StackResolver "lts-10.0"
  , _scPackages = NE.fromList
    [ StackPackage (PlFilePath "client") False Nothing
    , StackPackage (PlFilePath "server") False Nothing ]
  }

packageLocationsBs :: ByteString
packageLocationsBs = T.encodeUtf8 [st|
resolver: lts-10.0
packages:
  - location: humble/package
    subdirs:
    - client
    - server
  - location: https://example.com/foo/bar/baz-0.0.2.tar.gz
    extra-dep: true
    subdirs:
    - extra
  - location:
      git: git@github.com:example/mega-repo
      commit: 6a86ee32e5b869a877151f74064572225e1a0000
    subdirs:
      - subdir1
      - subdir2
    extra-dep: true
|]

packageLocationsConf :: StackConfig
packageLocationsConf = StackConfig
  { _scResolver = StackResolver "lts-10.0"
  , _scPackages = NE.fromList
    [ StackPackage (PlFilePath "humble/package") False (Just "client")
    , StackPackage (PlFilePath "humble/package") False (Just "server")
    , StackPackage (PlUri $ stUri "https://example.com/foo/bar/baz-0.0.2.tar.gz") True (Just "extra")
    , StackPackage (PlRepo $ Repo (Vcs "git") "git@github.com:example/mega-repo" "6a86ee32e5b869a877151f74064572225e1a0000") True (Just "subdir1")
    , StackPackage (PlRepo $ Repo (Vcs "git") "git@github.com:example/mega-repo" "6a86ee32e5b869a877151f74064572225e1a0000") True (Just "subdir2")
    ]
  }

packageExtraDepsBs :: ByteString
packageExtraDepsBs = T.encodeUtf8 [st|
resolver: lts-10.0
extra-deps:
  - acme-missiles-0.3
  - humble-package-1
|]

packageExtraDepsConf :: StackConfig
packageExtraDepsConf = StackConfig
  { _scResolver = StackResolver "lts-10.0"
  , _scPackages = NE.fromList
    [ StackPackage (PlFilePath ".") False Nothing
    , StackPackage (PlIndex $ PackageIndex "acme-missiles-0.3" Nothing) True Nothing
    , StackPackage (PlIndex $ PackageIndex "humble-package-1" Nothing) True Nothing ]
  }

packageMixedBs :: ByteString
packageMixedBs = T.encodeUtf8 [st|
resolver: lts-10.0
packages:
  - client
  - server
extra-deps:
  - acme-missiles-0.3
  - humble-package-1
|]

packageMixedConf :: StackConfig
packageMixedConf = StackConfig
  { _scResolver = StackResolver "lts-10.0"
  , _scPackages = NE.fromList
    [ StackPackage (PlFilePath "client") False Nothing
    , StackPackage (PlFilePath "server") False Nothing
    , StackPackage (PlIndex $ PackageIndex "acme-missiles-0.3" Nothing) True Nothing
    , StackPackage (PlIndex $ PackageIndex "humble-package-1" Nothing) True Nothing ]
  }

packageExtraDepsNewBs :: ByteString
packageExtraDepsNewBs = T.encodeUtf8 [st|
resolver: lts-10.0
extra-deps:
  - vendor/lib
  - acme-missiles-0.3
  - git: git@github.com:yesodweb/wai
    commit: 2f8a8e1b771829f4a8a77c0111352ce45a14c30f
    subdirs:
    - auto-update
    - wai
  - https://example.com/foo/bar/baz-0.0.2.tar.gz
  - archive: http://github.com/yesodweb/wai/archive/2f8a8e1b771829f4a8a77c0111352ce45a14c30f.zip
    subdirs:
    - wai
    - warp
  - archive: ../acme-missiles-0.3.tar.gz
    sha256: e563d8b524017a06b32768c4db8eff1f822f3fb22a90320b7e414402647b735b
|]

packageExtraDepsNewConf :: StackConfig
packageExtraDepsNewConf = StackConfig
  { _scResolver = StackResolver "lts-10.0"
  , _scPackages = NE.fromList
    [ StackPackage (PlFilePath ".") False Nothing
    , StackPackage (PlFilePath "vendor/lib") True Nothing
    , StackPackage (PlIndex $ PackageIndex "acme-missiles-0.3" Nothing) True Nothing
    , StackPackage (PlRepo $ Repo (Vcs "git") "git@github.com:yesodweb/wai" "2f8a8e1b771829f4a8a77c0111352ce45a14c30f") True (Just "auto-update")
    , StackPackage (PlRepo $ Repo (Vcs "git") "git@github.com:yesodweb/wai" "2f8a8e1b771829f4a8a77c0111352ce45a14c30f") True (Just "wai")
    , StackPackage (PlUri $ stUri "https://example.com/foo/bar/baz-0.0.2.tar.gz") True Nothing
    , StackPackage (PlUri $ stUri "http://github.com/yesodweb/wai/archive/2f8a8e1b771829f4a8a77c0111352ce45a14c30f.zip") True (Just "wai")
    , StackPackage (PlUri $ stUri "http://github.com/yesodweb/wai/archive/2f8a8e1b771829f4a8a77c0111352ce45a14c30f.zip") True (Just "warp")
    , StackPackage (PlFilePath "../acme-missiles-0.3.tar.gz") True Nothing
    ]
  }
