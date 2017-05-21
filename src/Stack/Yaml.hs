{-# LANGUAGE OverloadedStrings #-}

module Stack.Yaml where

import Control.Applicative
import Data.Aeson
import Data.Text as T

data Package
  = Simple Text
  | LocationSimple (Location Text)
  | LocationGit (Location Git)
  deriving (Eq, Show)

instance FromJSON Package where
  parseJSON v =
    (Simple <$> parseJSON v) <|>
    (LocationSimple <$> parseJSON v) <|>
    (LocationGit <$> parseJSON v)

data Location a = Location
  { _lLocation :: a
  , _lExtraDep :: Maybe Bool
  } deriving (Eq, Show)

instance FromJSON a => FromJSON (Location a) where
  parseJSON = withObject "Location" $ \v ->
    Location <$>
    v .: "location" <*>
    v .:? "extra-dep"

data Git = Git
  { _rGit    :: Text
  , _rCommit :: Text
  } deriving (Eq, Show)

instance FromJSON Git where
  parseJSON = withObject "Git" $ \v ->
    Git <$>
    v .: "git" <*>
    v .: "commit"

data Config = Config
  { _cResolver :: Text
  , _cPackages :: [Package]
  , _cExtraDeps :: [Text]
  } deriving (Eq, Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v ->
    Config <$>
    v .: "resolver" <*>
    v .: "packages" <*>
    v .: "extra-deps"
