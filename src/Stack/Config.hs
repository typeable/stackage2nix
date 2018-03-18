{-# LANGUAGE ViewPatterns #-}

module Stack.Config where

import Control.Lens
import Data.Bifunctor
import Data.ByteString as BS
import Data.Coerce
import Data.Foldable as F
import Data.List.NonEmpty as NE
import Data.Maybe
import Data.Semigroup
import Data.Text as T
import Data.Yaml
import Network.URI
import Stack.Config.Yaml as Yaml
import Stack.Types
import System.FilePath as FilePath


newtype StackResolver = StackResolver { fromStackResolver :: Text }
  deriving (Eq, Ord, Show)

makePrisms ''StackResolver

unStackResolver :: StackResolver -> String
unStackResolver = T.unpack . fromStackResolver

newtype Vcs = Vcs { fromVcs :: Text }
  deriving (Eq, Ord, Show)

data Repo = Repo
  { _rVcs    :: Vcs
  , _rUri    :: Text
  , _rCommit :: Text
  } deriving (Eq, Ord, Show)

makeLenses ''Repo

data PackageRevision
  = PrSha256 Text
  | PrRev Text
  deriving (Eq, Ord, Show)

makePrisms ''PackageRevision

data PackageIndex = PackageIndex
  { _piNameVersion :: Text
  , _piRevision    :: Maybe PackageRevision
  } deriving (Eq, Ord, Show)

makeLenses ''PackageIndex

data PackageLocation
  = StackIndex PackageIndex
  | StackFilePath FilePath
  | StackUri URI
  | StackRepo Repo
  deriving (Eq, Ord, Show)

makePrisms ''PackageLocation

data StackPackage = StackPackage
  { _spLocation :: PackageLocation
  , _spExtraDep :: Bool
    -- | Subdirectory containing the cabal file if any specified.
  , _spDir      :: Maybe FilePath
  } deriving (Eq, Ord, Show)

makeLenses ''StackPackage

data StackConfig = StackConfig
  { _scResolver  :: StackResolver
  , _scPackages  :: NonEmpty StackPackage
  } deriving (Eq, Ord, Show)

makeLenses ''StackConfig

fromYamlConfig :: Yaml.Config -> StackConfig
fromYamlConfig c = StackConfig{..}
  where
    _scResolver = coerce $ c ^. cResolver
    -- 'NE.nub' replicates Stack behavior of having a Set of packages rather than list
    _scPackages = NE.nub $ F.foldl' (<>) nePackages yamlExtraDeps
    nePackages = fromMaybe (pure defaultPackage)
      $ fmap sconcat
      $ NE.nonEmpty yamlPackages
    yamlPackages   = fromYamlPackage packagesSimplePath False
      <$> fromMaybe mempty (c ^. cPackages)
    yamlExtraDeps  = fromYamlPackage extraDepsSimplePath True
      <$> fromMaybe mempty (c ^. cExtraDeps)
    defaultPackage = StackPackage (StackFilePath ".") False Nothing

-- | Simple string in a packages section can be an URI or FilePath location
packagesSimplePath :: Text -> StackPackage
packagesSimplePath t = StackPackage (parseSimplePath t) False Nothing

-- | Parse location as URI or a FilePath
parseSimplePath :: Text -> PackageLocation
parseSimplePath (T.unpack -> p) = maybe (StackFilePath p) StackUri $ parseURI p

-- | Simple string in an extra-deps section can be an URI or FilePath or PackageIndex name-version
extraDepsSimplePath :: Text -> StackPackage
extraDepsSimplePath t = StackPackage loc True Nothing
  where
    loc = maybe (parsePackageIndex t) StackUri $ parseURI (T.unpack t)

-- TODO: support new package index format with hash and revision
parsePackageIndex :: Text -> PackageLocation
parsePackageIndex t = if isFilePath t
  then StackFilePath (T.unpack t)
  else StackIndex $ PackageIndex t Nothing

-- Determine wether package is file path
isFilePath :: Text -> Bool
isFilePath = T.any (== FilePath.pathSeparator)

-- | A single 'Yaml.Package' can result in multiple actual
-- packages if it has multiple subdirs specified.
fromYamlPackage
  :: (Text -> StackPackage)
  -> Bool
  -> Yaml.Package
  -> NonEmpty StackPackage
fromYamlPackage fromSimple isExtraDep = \case
  Yaml.PSimple p                                ->
    pure (fromSimple p)
  Yaml.PLocationSimple (Location p extraDep ms) ->
    unroll ms $ StackPackage (parseSimplePath p) (fromMaybe False extraDep)
  Yaml.PLocationGit loc                         ->
    mkStackPackageRepo fromYamlGit loc
  Yaml.PLocationHg loc                          ->
    mkStackPackageRepo fromYamlHg loc
  Yaml.PArchive a                               ->
    unroll (a ^. aSubdirs) $ StackPackage (a ^. aArchive . to parseSimplePath) isExtraDep
  Yaml.PNewGit ng                               -> unroll (ng ^. ngSubdirs)
    $ StackPackage (StackRepo $ fromYamlNewGit ng) isExtraDep
  Yaml.PNewHg nh                                -> unroll (nh ^. nhSubdirs)
    $ StackPackage (StackRepo $ fromYamlNewHg nh) isExtraDep
  where
    mkStackPackageRepo f loc = unroll (loc ^. lSubdirs)
      $ StackPackage (StackRepo (loc ^. lLocation . to f)) (fromMaybe isExtraDep $ loc ^. lExtraDep)
    -- Each package gets a single directory with cabal file in it. If
    -- it's not specified, path is empty.
    unroll subs p = case subs of
      Just (x : xs) -> NE.map (p . Just) (x :| xs)
      _ -> p Nothing :| []

fromYamlGit :: Yaml.Git -> Repo
fromYamlGit yg = Repo
  { _rVcs    = Vcs "git"
  , _rUri    = yg ^. gGit
  , _rCommit = yg ^. gCommit }

fromYamlHg :: Yaml.Hg -> Repo
fromYamlHg yh = Repo
  { _rVcs    = Vcs "hg"
  , _rUri    = yh ^. hHg
  , _rCommit = yh ^. hCommit }

fromYamlNewGit :: NewGit -> Repo
fromYamlNewGit ng = Repo
  { _rVcs    = Vcs "git"
  , _rUri    = ng ^. ngGit
  , _rCommit = ng ^. ngCommit }

fromYamlNewHg :: NewHg -> Repo
fromYamlNewHg nh = Repo
  { _rVcs    = Vcs "hg"
  , _rUri    = nh ^. nhHg
  , _rCommit = nh ^. nhCommit }

readStackConfig :: StackYaml -> IO (Either String StackConfig)
readStackConfig stackYaml = do
  let
    relativeToStackYaml = \case
      StackFilePath p -> StackFilePath $ stackYaml ^. syDirName </> p
      packageLocation -> packageLocation
    mkStackConfig = over (scPackages . traversed . spLocation) relativeToStackYaml
      . fromYamlConfig
  second mkStackConfig . decodeEither <$> BS.readFile (stackYaml ^. syFilePath)
