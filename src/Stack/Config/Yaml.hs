{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Stack.Config.Yaml where

import Control.Applicative
import Control.Lens
import Data.Aeson
import Data.Text as T

data Location a = Location
  { _lLocation :: a
  , _lExtraDep :: Maybe Bool
  } deriving (Eq, Show)

makeLenses ''Location

instance FromJSON a => FromJSON (Location a) where
  parseJSON = withObject "Location" $ \v ->
    Location <$>
    v .: "location" <*>
    v .:? "extra-dep"

data Git = Git
  { _gGit    :: Text
  , _gCommit :: Text
  } deriving (Eq, Show)

makeLenses ''Git

instance FromJSON Git where
  parseJSON = withObject "Git" $ \v ->
    Git <$>
    v .: "git" <*>
    v .: "commit"

data Package
  = Simple Text
  | LocationSimple (Location Text)
  | LocationGit (Location Git)
  deriving (Eq, Show)
makePrisms ''Package

instance FromJSON Package where
  parseJSON v =
    (Simple <$> parseJSON v) <|>
    (LocationSimple <$> parseJSON v) <|>
    (LocationGit <$> parseJSON v)

data Config = Config
  { _cResolver  :: Text
  , _cPackages  :: [Package]
  , _cExtraDeps :: [Text]
  } deriving (Eq, Show)

makeLenses ''Config

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v ->
    Config <$>
    v .:  "resolver"          <*>
    v .:? "packages"   .!= [] <*>
    v .:? "extra-deps" .!= []
