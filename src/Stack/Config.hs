{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Stack.Config where

import Control.Lens
import Data.Bifunctor
import Data.ByteString as BS
import Data.Coerce
import Data.Maybe
import Data.Text as T
import Data.Yaml
import Stack.Config.Yaml as Yaml

newtype StackResolver = StackResolver { fromStackResolver :: Text }
  deriving (Eq, Ord, Show)

makePrisms ''StackResolver

unStackResolver :: StackResolver -> String
unStackResolver = T.unpack . fromStackResolver

data RepoGit = RepoGit
  { _rgUri    :: Text
  , _rgCommit :: Text
  } deriving (Eq, Ord, Show)

makeLenses ''RepoGit

data StackLocation
  = StackPath Text
  | StackRepoGit RepoGit
  deriving (Eq, Ord, Show)

makePrisms ''StackLocation

data StackPackage = StackPackage
  { _spLocation :: StackLocation
  , _spExtraDep :: Bool
  } deriving (Eq, Ord, Show)

makeLenses ''StackPackage

newtype StackExtraDep = StackExtraDep { fromStackExtraDep :: Text }
  deriving (Eq, Ord, Show)

makePrisms ''StackExtraDep

unExtraDep :: StackExtraDep -> String
unExtraDep = T.unpack . fromStackExtraDep

data StackConfig = StackConfig
  { _scResolver  :: StackResolver
  , _scPackages  :: [StackPackage]
  , _scExtraDeps :: [StackExtraDep]
  } deriving (Eq, Ord, Show)

makeLenses ''StackConfig

fromYamlConfig :: Yaml.Config -> StackConfig
fromYamlConfig c = StackConfig{..}
  where
    _scResolver = coerce $ c ^. cResolver
    _scPackages = fromYamlPackage <$> c ^. cPackages
    _scExtraDeps = StackExtraDep <$> c ^. cExtraDeps

fromYamlPackage :: Yaml.Package -> StackPackage
fromYamlPackage = \case
  Yaml.Simple path                                  ->
    StackPackage (StackPath path) False
  Yaml.LocationSimple (Yaml.Location path extraDep) ->
    StackPackage (StackPath path) (fromMaybe False extraDep)
  Yaml.LocationGit (Location git extraDep) ->
    StackPackage (StackRepoGit $ fromYamlGit git) (fromMaybe False extraDep)

fromYamlGit :: Yaml.Git -> RepoGit
fromYamlGit yg = RepoGit{..}
  where
    _rgUri = yg ^. gGit
    _rgCommit = yg ^. gCommit

readStackConfig :: FilePath -> IO (Either String StackConfig)
readStackConfig stackYaml =
  second fromYamlConfig . decodeEither <$> BS.readFile stackYaml
