module Distribution.Nixpkgs.Haskell.FromStack where

import AllCabalHashes
import Control.Lens
import Data.Set.Lens
import Distribution.Compiler (CompilerInfo(..))
import Distribution.System (Platform(..))
import Distribution.Package (PackageName, PackageIdentifier(..), Dependency(..))
import Distribution.PackageDescription
import Distribution.Nixpkgs.Haskell.FromStack.Package
import Distribution.Nixpkgs.Haskell.PackageSourceSpec
import Distribution.Nixpkgs.Haskell.FromCabal
import Distribution.Nixpkgs.Haskell.Derivation
import Stackage.BuildPlan
import Stackage.Types (CabalFileInfo(..),PackageConstraints(..), DepInfo(..), SimpleDesc(..), TestState(..))
import qualified Distribution.Nixpkgs.Meta as Nix

import qualified Data.Map as Map
import qualified Data.Set as Set

data PackageSetConfig = PackageSetConfig
  { haskellResolver :: HaskellResolver
  , nixpkgsResolver :: NixpkgsResolver
  , packageLoader   :: Maybe SHA1Hash -> PackageIdentifier -> IO Package
  , targetPlatform  :: Platform
  , targetCompiler  :: CompilerInfo }

data PackageConfig = PackageConfig
  { enableCheck   :: Bool
  , enableHaddock :: Bool }

removeTests :: GenericPackageDescription -> GenericPackageDescription
removeTests gd = gd { condTestSuites = [] }

planDependencies :: PackagePlan -> [Dependency]
planDependencies = map makeDependency . Map.toList . sdPackages . ppDesc
 where
  makeDependency (name, depInfo) = Dependency name (diRange depInfo)

buildNodeM :: PackageSetConfig -> PackageConfig -> PackageName -> PackagePlan -> IO Node
buildNodeM conf pconf name plan = do
  let
    cabalHashes = maybe mempty cfiHashes $ ppCabalFileInfo plan
    mGitSha1 = Map.lookup "GitSHA1" cabalHashes
  pkg <- packageLoader conf mGitSha1 $ PackageIdentifier name (ppVersion plan)
  pure . mkNode $ fromPackage conf pconf plan pkg

fromPackage :: PackageSetConfig -> PackageConfig -> PackagePlan -> Package -> Derivation
fromPackage conf pconf plan pkg =
  let
    constraints = ppConstraints plan
    testsEnabled =
      enableCheck pconf &&
      pcTests constraints == ExpectSuccess
    haddocksEnabled =
      enableHaddock pconf &&
      pcHaddocks constraints == ExpectSuccess &&
      not (Set.null (sdModules (ppDesc plan)))
    configureTests
      | pcTests constraints == Don'tBuild = removeTests
      | otherwise = id
    flags = Map.toList (pcFlagOverrides constraints)
    (descr, missingDeps) = finalizeGenericPackageDescription
      (haskellResolver conf)
      (targetPlatform conf)
      (targetCompiler conf)
      flags
      (planDependencies plan)
      (configureTests (pkgCabal pkg))
    genericDrv = fromPackageDescription
      (haskellResolver conf)
      (nixpkgsResolver conf)
      missingDeps
      flags
      descr
    depName (Dependency name _) = name
    testDeps = setOf
      ( to testSuites . folded
        . to testBuildInfo . to targetBuildDepends
        . folded . to depName )
    hasIntersection a b = not . null $ Set.intersection a b
    brokenEnabled =
      testsEnabled &&
      not (null missingDeps) &&
      setOf (folded . to depName) missingDeps `hasIntersection` testDeps descr
  in
    genericDrv
      & src .~ pkgSource pkg
      & doCheck .~ testsEnabled
      & runHaddock .~ haddocksEnabled
      & metaSection . Nix.broken .~ brokenEnabled
