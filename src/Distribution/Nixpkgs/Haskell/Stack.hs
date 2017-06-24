module Distribution.Nixpkgs.Haskell.Stack where

import Control.Lens
import Data.Text as T
import Distribution.Compiler as Compiler
import Distribution.Nixpkgs.Fetch
import Distribution.Nixpkgs.Haskell.Derivation
import Distribution.Nixpkgs.Haskell.FromCabal as FromCabal
import Distribution.Nixpkgs.Haskell.FromStack.Package as FromStack
import Distribution.Nixpkgs.Haskell.PackageSourceSpec as PackageSourceSpec
import Distribution.PackageDescription as PackageDescription
import Distribution.System as System
import Language.Nix as Nix
import Network.URI as URI
import Stack.Config

newtype HackageDb = HackageDb { fromHackageDB :: T.Text }
  deriving (Eq, Ord, Show)

makePrisms ''HackageDb

unHackageDb :: HackageDb -> String
unHackageDb = T.unpack . fromHackageDB

data PackageConfig = PackageConfig
  { _pcHaskellResolver :: !HaskellResolver
  , _pcNixpkgsResolver :: !NixpkgsResolver
  , _pcTargetPlatform  :: !Platform
  , _pcTargetCompiler  :: !CompilerInfo
  , _pcFlagAssignment  :: !FlagAssignment
  }

makeLenses ''PackageConfig

mkPackageConfig :: Platform -> CompilerId -> PackageConfig
mkPackageConfig platform compilerId = PackageConfig
  { _pcHaskellResolver = const True
  , _pcNixpkgsResolver = \i -> Just (Nix.binding # (i, Nix.path # [i]))
  , _pcTargetPlatform  = platform
  , _pcTargetCompiler  = unknownCompilerInfo compilerId NoAbiTag
  , _pcFlagAssignment  = []
  }

getStackPackageFromDb
  :: Maybe HackageDb
  -> StackPackage
  -> IO Package
getStackPackageFromDb optHackageDb stackPackage =
  PackageSourceSpec.getPackage
    (unHackageDb <$> optHackageDb)
    (stackLocationToSource $ stackPackage ^. spLocation)

stackLocationToSource :: PackageLocation -> Source
stackLocationToSource = \case
  HackagePackage p -> Source
    { sourceUrl      = "cabal://" ++ T.unpack p
    , sourceRevision = mempty
    , sourceHash     = UnknownHash
    , sourceCabalDir = mempty }
  StackFilePath p  -> Source
    { sourceUrl      = p
    , sourceRevision = mempty
    , sourceHash     = UnknownHash
    , sourceCabalDir = mempty }
  StackUri uri     -> Source
    { sourceUrl      = URI.uriToString id uri mempty
    , sourceRevision = mempty
    , sourceHash     = UnknownHash
    , sourceCabalDir = mempty }
  StackRepoGit rg  -> Source
    { sourceUrl      = T.unpack $ rg ^. rgUri
    , sourceRevision = T.unpack $ rg ^. rgCommit
    , sourceHash     = UnknownHash
    , sourceCabalDir = mempty }

packageDerivation
  :: PackageConfig
  -> Maybe HackageDb
  -> StackPackage
  -> IO Derivation
packageDerivation conf optHackageDb stackPackage = do
  pkg <- getStackPackageFromDb optHackageDb stackPackage
  pure . overrideStackageDerivation
    $ genericPackageDerivation conf pkg
    & src .~ pkgSource pkg
    & doCheck .~ False
    & runHaddock .~ False

genericPackageDerivation
  :: PackageConfig
  -> Package
  -> Derivation
genericPackageDerivation conf pkg =
  FromCabal.fromGenericPackageDescription
    (conf ^. pcHaskellResolver)
    (conf ^. pcNixpkgsResolver)
    (conf ^. pcTargetPlatform)
    (conf ^. pcTargetCompiler)
    (conf ^. pcFlagAssignment)
    []
    (pkgCabal pkg)

-- Stackage packages set only consistent for packages with their library and
-- executable dependencies and completely ignores test dependencies.
overrideStackageDerivation :: Derivation -> Derivation
overrideStackageDerivation node = node
   & doCheck .~ False
   & runHaddock .~ False

overrideStackageNode :: Node -> Node
overrideStackageNode = over nodeDerivation overrideStackageDerivation
