module LtsHaskell where

import AllCabalHashes
import Control.Lens
import Control.Monad.Catch
import Data.Maybe
import Data.Text as T
import Distribution.Text as Text (display)
import Control.Monad
import Distribution.Compiler (CompilerInfo(..), AbiTag(NoAbiTag), CompilerId(..), CompilerFlavor(GHC))
import Distribution.System (Platform(..))
import Distribution.Package (PackageName, PackageIdentifier(..), Dependency(..))
import Distribution.Nixpkgs.Haskell.PackageSourceSpec as PackageSourceSpec
import Distribution.Nixpkgs.Haskell.FromStack
import Distribution.Nixpkgs.Haskell.Stack
import Distribution.Nixpkgs.Fetch
import Distribution.Nixpkgs.PackageMap (readNixpkgPackageMap, resolve)
import Distribution.Version (Version, withinRange)
import Language.Haskell.Extension (Language(Haskell98, Haskell2010))
import Language.Nix
import Stack.Config
import Stackage.BuildPlan
import Stackage.Types (SystemInfo(..))
import System.FilePath as Path

import qualified Distribution.Nixpkgs.Haskell.Hackage as DB
import qualified Data.Yaml as Yaml
import qualified Data.Map as Map
import qualified Data.Set as Set


loadBuildPlan :: FilePath -> IO BuildPlan
loadBuildPlan = Yaml.decodeFileEither >=> \case
  Left err -> fail $ "Failed to parse stackage build plan: " ++ show err
  Right bp -> return bp

getPackageFromRepo :: FilePath -> Maybe SHA1Hash -> PackageIdentifier -> IO Package
getPackageFromRepo allCabalHashesPath mSha1Hash pkgId = do
  (pkgDesc, _) <- case mSha1Hash of
    Just sha1 ->
      readPackageByHash allCabalHashesPath sha1
      `catchAll` const (readPackageByName allCabalHashesPath pkgId)
    Nothing   -> readPackageByName allCabalHashesPath pkgId
  meta <- readPackageMeta allCabalHashesPath pkgId
  let
    tarballSHA256 = fromMaybe
      (error (display pkgId ++ ": meta data has no SHA256 hash for the tarball"))
      (view (mHashes . at "SHA256") meta)
    source = DerivationSource "url" ("mirror://hackage/" ++ display pkgId ++ ".tar.gz") "" tarballSHA256 Nothing
  return $ Package source False pkgDesc

getPackageFromDb :: DB.HackageDB -> PackageIdentifier -> IO Package
getPackageFromDb hackageDb pkgId =
  getStackPackageFromDb hackageDb
  $ StackPackage (PlIndex $ PackageIndex (T.pack $ Text.display pkgId) Nothing) True Nothing

loadPackage :: DB.HackageDB -> FilePath -> Maybe SHA1Hash -> PackageIdentifier -> IO Package
loadPackage hackageDb allCabalHashesPath mSha1Hash pkgId =
  getPackageFromRepo allCabalHashesPath mSha1Hash pkgId
  `catchIOError` const (getPackageFromDb hackageDb pkgId)

ghcCompilerInfo :: Version -> CompilerInfo
ghcCompilerInfo v = CompilerInfo
  { compilerInfoId = CompilerId GHC v
  , compilerInfoAbiTag = NoAbiTag
  , compilerInfoCompat = Just []
  , compilerInfoLanguages = Just [Haskell98, Haskell2010]
  , compilerInfoExtensions = Nothing
  }

buildPlanContainsDependency :: Map.Map PackageName Version -> Dependency -> Bool
buildPlanContainsDependency packageVersions (Dependency depName versionRange) =
  maybe False (`withinRange` versionRange) $ Map.lookup depName packageVersions

buildPackageSetConfig
  :: DB.HackageDB
  -> FilePath
  -> FilePath
  -> BuildPlan
  -> IO PackageSetConfig
buildPackageSetConfig hackageDb optAllCabalHashes optNixpkgsRepository buildPlan = do
  nixpkgs <- readNixpkgPackageMap optNixpkgsRepository Nothing
  let
    systemInfo      = bpSystemInfo buildPlan
    packageVersions = fmap ppVersion (bpPackages buildPlan) `Map.union` siCorePackages systemInfo
  return PackageSetConfig
    { packageLoader   = loadPackage hackageDb optAllCabalHashes
    , targetPlatform  = Platform (siArch systemInfo) (siOS systemInfo)
    , targetCompiler  = ghcCompilerInfo (siGhcVersion systemInfo)
    , nixpkgsResolver = resolve (Map.map (Set.map (over path ("pkgs":))) nixpkgs)
    , haskellResolver = buildPlanContainsDependency packageVersions }

-- Path to Yaml build definition file in lts-haskell repository
buildPlanFilePath :: FilePath -> StackResolver -> FilePath
buildPlanFilePath ltsRepoDir resolver =
  ltsRepoDir Path.</>
  (resolver ^. to unStackResolver) Path.<.>
  ".yaml"
