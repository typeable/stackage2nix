{-# LANGUAGE ViewPatterns #-}

module Stack.Config where

import Control.Lens
import Data.Bifunctor
import Data.ByteString as BS
import Data.Coerce
import Data.Maybe
import Data.Text as T
import Data.Yaml
import Network.URI
import Stack.Config.Yaml as Yaml
import Stack.Types
import System.FilePath


newtype StackResolver = StackResolver { fromStackResolver :: Text }
  deriving (Eq, Ord, Show)

makePrisms ''StackResolver

unStackResolver :: StackResolver -> String
unStackResolver = T.unpack . fromStackResolver

data RepoGit = RepoGit
  { _rgUri    :: !Text
  , _rgCommit :: !Text
  } deriving (Eq, Ord, Show)

makeLenses ''RepoGit

data PackageLocation
  = HackagePackage Text
  | StackFilePath FilePath
  | StackUri URI
  | StackRepoGit RepoGit
  deriving (Eq, Ord, Show)

makePrisms ''PackageLocation

data StackPackage = StackPackage
  { _spLocation :: !PackageLocation
  , _spExtraDep :: !Bool
  } deriving (Eq, Ord, Show)

makeLenses ''StackPackage

data StackConfig = StackConfig
  { _scResolver  :: !StackResolver
  , _scPackages  :: ![StackPackage]
  } deriving (Eq, Ord, Show)

makeLenses ''StackConfig

fromYamlConfig :: Yaml.Config -> StackConfig
fromYamlConfig c = StackConfig{..}
  where
    _scResolver = coerce $ c ^. cResolver
    _scPackages =
      (fromYamlPackage <$> c ^. cPackages . to (fromMaybe mempty)) ++
      (fromYamlExtraDep <$> c ^. cExtraDeps . to (fromMaybe mempty))

fromYamlPackage :: Yaml.Package -> StackPackage
fromYamlPackage = \case
  Yaml.Simple p                                  ->
    StackPackage (parseSimplePath p) False
  Yaml.LocationSimple (Yaml.Location p extraDep) ->
    StackPackage (parseSimplePath p) (fromMaybe False extraDep)
  Yaml.LocationGit (Location git extraDep)       ->
    StackPackage (StackRepoGit $ fromYamlGit git) (fromMaybe False extraDep)
  where
    parseSimplePath (T.unpack -> p) = maybe (StackFilePath p) StackUri $ parseURI p

fromYamlExtraDep :: Text -> StackPackage
fromYamlExtraDep = flip StackPackage True . HackagePackage

fromYamlGit :: Yaml.Git -> RepoGit
fromYamlGit yg = RepoGit{..}
  where
    _rgUri = yg ^. gGit
    _rgCommit = yg ^. gCommit

readStackConfig :: StackYaml -> IO (Either String StackConfig)
readStackConfig stackYaml = do
  let
    relativeToStackYaml = \case
      StackFilePath p -> StackFilePath $ stackYaml ^. syDirName </> p
      packageLocation -> packageLocation
    mkStackConfig = over (scPackages . traversed . spLocation) relativeToStackYaml
      . fromYamlConfig
  second mkStackConfig . decodeEither <$> BS.readFile (stackYaml ^. syFilePath)
