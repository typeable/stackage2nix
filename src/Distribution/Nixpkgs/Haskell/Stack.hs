module Distribution.Nixpkgs.Haskell.Stack where

import Control.Lens
import Data.Maybe (fromMaybe)
import Data.Text as T
import Distribution.Compiler as Compiler
import Distribution.Nixpkgs.Fetch
import Distribution.Nixpkgs.Haskell.Derivation
import Distribution.Nixpkgs.Haskell.FromCabal as FromCabal
import Distribution.Nixpkgs.Haskell.FromStack as FromStack
import Distribution.Nixpkgs.Haskell.Hackage as DB
import Distribution.Nixpkgs.Haskell.PackageSourceSpec as PackageSourceSpec
import Distribution.PackageDescription as PackageDescription
import Distribution.System as System
import Network.URI as URI
import Stack.Config



data StackPackagesConfig = StackPackagesConfig
  { _spcHaskellResolver   :: HaskellResolver
  , _spcNixpkgsResolver   :: NixpkgsResolver
  , _spcTargetPlatform    :: Platform
  , _spcTargetCompiler    :: CompilerInfo
  , _spcFlagAssignment    :: FlagAssignment
  , _spcDoCheckPackages   :: Bool
  , _spcDoHaddockPackages :: Bool
  }

makeLenses ''StackPackagesConfig

getStackPackageFromDb :: DB.HackageDB -> StackPackage -> IO Package
getStackPackageFromDb hackageDb stackPackage = PackageSourceSpec.getPackage' PackageYamlHpack
                                                                             False
                                                                             (pure hackageDb)
                                                                             source
  where
    source = stackLocationToSource (stackPackage ^. spLocation) (stackPackage ^. spDir)

stackLocationToSource
  :: PackageLocation
     -- ^ Subdirectory in the package containing cabal file.
  -> Maybe FilePath
  -> Source
stackLocationToSource pl mCabalDir = case pl of
  PlIndex p    -> Source
    { sourceUrl      = "cabal://" ++ T.unpack (p ^. piNameVersion)
    , sourceRevision = mempty
    , sourceHash     = UnknownHash
    , sourceCabalDir = cabalDir }
  PlFilePath p -> Source
    { sourceUrl      = p
    , sourceRevision = mempty
    , sourceHash     = UnknownHash
    , sourceCabalDir = cabalDir }
  PlUri uri    -> Source
    { sourceUrl      = URI.uriToString id uri mempty
    , sourceRevision = mempty
    , sourceHash     = UnknownHash
    , sourceCabalDir = cabalDir }
  PlRepo r     -> Source
    { sourceUrl      = T.unpack $ r ^. rUri
    , sourceRevision = T.unpack $ r ^. rCommit
    , sourceHash     = UnknownHash
    , sourceCabalDir = cabalDir }
  where
    cabalDir = fromMaybe mempty mCabalDir

packageDerivation
  :: StackPackagesConfig
  -> DB.HackageDB
  -> StackPackage
  -> IO Derivation
packageDerivation conf hackageDb stackPackage = do
  pkg <- getStackPackageFromDb hackageDb stackPackage
  let
    drv = genericPackageDerivation conf pkg
      & subpath %~ flip fromMaybe (stackPackage ^. spDir)
    pconf = PackageConfig
      { enableCheck   = conf ^. spcDoCheckPackages
      , enableHaddock = conf ^. spcDoHaddockPackages }
  return $ finalizePackage pkg pconf drv

genericPackageDerivation
  :: StackPackagesConfig
  -> Package
  -> Derivation
genericPackageDerivation conf pkg =
  FromCabal.fromGenericPackageDescription
    (conf ^. spcHaskellResolver)
    (conf ^. spcNixpkgsResolver)
    (conf ^. spcTargetPlatform)
    (conf ^. spcTargetCompiler)
    (conf ^. spcFlagAssignment)
    []
    (pkgCabal pkg)
