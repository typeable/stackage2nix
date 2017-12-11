{-# LANGUAGE ViewPatterns #-}

module Stack.Config where

import Control.Lens
import Data.Bifunctor
import Data.ByteString as BS
import Data.Coerce
import Data.Foldable as F
import Data.List.NonEmpty as NE
import Data.Maybe
import Data.Semigroup (sconcat)
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

newtype Vcs = Vcs { fromVcs :: Text }
  deriving (Eq, Ord, Show)

data Repo = Repo
  { _rVcs    :: !Vcs
  , _rUri    :: !Text
  , _rCommit :: !Text
  } deriving (Eq, Ord, Show)

makeLenses ''Repo

data PackageLocation
  = HackagePackage Text
  | StackFilePath FilePath
  | StackUri URI
  | StackRepo Repo
  deriving (Eq, Ord, Show)

makePrisms ''PackageLocation

data StackPackage = StackPackage
  { _spLocation :: !PackageLocation
  , _spExtraDep :: !Bool
    -- | Subdirectory containing the cabal file if any specified.
  , _spDir  :: !(Maybe FilePath)
  } deriving (Eq, Ord, Show)

makeLenses ''StackPackage

data StackConfig = StackConfig
  { _scResolver  :: !StackResolver
  , _scPackages  :: !(NonEmpty StackPackage)
  } deriving (Eq, Ord, Show)

makeLenses ''StackConfig

fromYamlConfig :: Yaml.Config -> StackConfig
fromYamlConfig c = StackConfig{..}
  where
    _scResolver = coerce $ c ^. cResolver
    _scPackages = F.foldr (NE.<|) neYamlPackages yamlExtraDeps
    neYamlPackages = fromMaybe (pure defaultPackage)
                   . fmap sconcat $ NE.nonEmpty yamlPackages
    yamlPackages   = fromYamlPackage <$> (fromMaybe mempty (c ^. cPackages))
    yamlExtraDeps  = fromYamlExtraDep <$> fromMaybe mempty (c ^. cExtraDeps)
    defaultPackage = StackPackage (StackFilePath ".") False Nothing


fromYamlPackage
  :: Yaml.Package
     -- ^ A single 'Yaml.Package' can result in multiple actual
     -- packages if it has multiple subdirs specified.
  -> NonEmpty StackPackage
fromYamlPackage = \case
  Yaml.Simple p                                  ->
    unroll Nothing $ StackPackage (parseSimplePath p) False
  Yaml.LocationSimple (Location p extraDep ms) ->
    unroll ms $ StackPackage (parseSimplePath p) (fromMaybe False extraDep)
  Yaml.LocationGit (Location git extraDep ms)       ->
    unroll ms $ StackPackage (StackRepo $ fromYamlGit git) (fromMaybe False extraDep)
  Yaml.LocationHg (Location hg extraDep ms)       ->
    unroll ms $ StackPackage (StackRepo $ fromYamlHg hg) (fromMaybe False extraDep)
  where
    parseSimplePath (T.unpack -> p) = maybe (StackFilePath p) StackUri $ parseURI p
    -- Each package gets a single directory with cabal file in it. If
    -- it's not specified, path is empty.
    unroll subs p = case subs of
      Just (x : xs) -> NE.map (p . Just) (x :| xs)
      _ -> p Nothing :| []

fromYamlExtraDep :: Text -> StackPackage
fromYamlExtraDep t = StackPackage (HackagePackage t) True Nothing

fromYamlGit :: Yaml.Git -> Repo
fromYamlGit yg = Repo{..}
  where
    _rVcs = Vcs "git"
    _rUri = yg ^. gGit
    _rCommit = yg ^. gCommit

fromYamlHg :: Yaml.Hg -> Repo
fromYamlHg yh = Repo{..}
  where
    _rVcs = Vcs "hg"
    _rUri = yh ^. hHg
    _rCommit = yh ^. hCommit

readStackConfig :: StackYaml -> IO (Either String StackConfig)
readStackConfig stackYaml = do
  let
    relativeToStackYaml = \case
      StackFilePath p -> StackFilePath $ stackYaml ^. syDirName </> p
      packageLocation -> packageLocation
    mkStackConfig = over (scPackages . traversed . spLocation) relativeToStackYaml
      . fromYamlConfig
  second mkStackConfig . decodeEither <$> BS.readFile (stackYaml ^. syFilePath)
