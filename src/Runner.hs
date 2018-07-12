module Runner ( run ) where

import Control.Lens
import Data.Foldable as F
import Data.List as L
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Distribution.Nixpkgs.Haskell.FromStack
import Distribution.Nixpkgs.Haskell.FromStack.Package
import Distribution.Nixpkgs.Haskell.Hackage as DB
import Distribution.Nixpkgs.Haskell.PackageSourceSpec (loadHackageDB)
import Distribution.Nixpkgs.Haskell.Stack
import Distribution.Nixpkgs.Haskell.Stack.PrettyPrinting as PP
import Distribution.Version (Version)
import Distribution.Compiler (AbiTag(..), unknownCompilerInfo)
import Distribution.Package (PackageName, mkPackageName, pkgName)
import Distribution.Text as Text (display)
import Language.Nix as Nix
import Options.Applicative
import Paths_stackage2nix ( version )
import Runner.Cli
import Stack.Config
import Stack.Types
import Stackage.Types
import System.IO (withFile, IOMode(..), hPutStrLn)
import Text.PrettyPrint.HughesPJClass (Doc, render)

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified LtsHaskell as LH

run :: IO ()
run = do
  opts <- execParser pinfo
  hackageDb <- loadHackageDB (opts ^. optHackageDb) Nothing
  let
    -- With optparse-applicative it seems there's no way to make
    -- 'StackageOptions' optional for Stackage override and required in
    -- other cases
    missingErr = error . ("Missing required parameter: " <>)
    sopts = StackageOptions
      (fromMaybe (missingErr "--all-cabal-hashes") (opts ^. optAllCabalHashesRepo) )
      (fromMaybe (missingErr "--lts-haskell") (opts ^. optLtsHaskellRepo))

  case opts ^. optConfigOrigin of
    -- Generate Stackage packages from stack.yaml resolver
    OriginStackYaml stackYaml buildTarget -> do
      stackConf <- either fail pure =<< readStackConfig stackYaml
      let
        stackResolver = stackConf ^. scResolver
        stackPackagesConfig = mkStackPackagesConfig opts
      stackConfPackages <- traverse (fmap mkNode . packageDerivation stackPackagesConfig hackageDb)
        $ stackConf ^. scPackages
      stackConfExtraDeps  <- traverse (fmap mkNode . packageDerivation stackPackagesConfig hackageDb)
        $ stackConf ^. scExtraDeps
      let
        stackConfAllPackages = maybe stackConfPackages ((<>) stackConfPackages)
          $ NE.nonEmpty stackConfExtraDeps

      case buildTarget of
        TargetStackageClosure -> do
          (buildPlanFile, buildPlan, packageSetConfig) <- loadStackageBuildPlan hackageDb stackResolver opts sopts
          let
            overrideConfig = mkOverrideConfig opts (siGhcVersion $ bpSystemInfo buildPlan)
            stackageSetConfig = if opts ^. optTweaks . tExtraDepsRevisionLatest
              then extraDepsLatestConfig stackConfAllPackages packageSetConfig
              else packageSetConfig
          stackagePackages <- buildStackagePackages stackageSetConfig buildPlan
          let
            reachable = reachableDeps stackConfAllPackages
            -- Nixpkgs generic-builder puts hscolour on path for all libraries
            withHscolour pkgs =
              let hscolour = F.find ((== "hscolour") . nodeName) stackagePackages
              in maybe pkgs (`Set.insert` pkgs) hscolour
            -- Find all reachable dependencies in stackage set to stick into
            -- stackage packages file. This is performed on the full stackage
            -- set rather than pruning stackage packages beforehand because
            -- stackage does not concern itself with build tools while cabal2nix
            -- does: pruning only after generating full set of packages allows
            -- us to make sure all those extra dependencies are explicitly
            -- listed as well.
            nodes = Set.toAscList
                $ withHscolour
                $ flip reachableDependencies stackagePackages
                -- Originally reachable nodes are root nodes
                $ L.filter (\n -> mkPackageName (nodeName n) `Set.member` reachable) stackagePackages
          writeOutFile buildPlanFile (opts ^. optOutStackagePackages)
            $ pPrintOutPackages (view nodeDerivation <$> nodes)
          writeOutFile buildPlanFile (opts ^. optOutStackageConfig)
            $ pPrintOutConfig (bpSystemInfo buildPlan) nodes
          writeOutFile (stackYaml ^. syFilePath) (opts ^. optOutDerivation)
            $ PP.overrideHaskellPackages overrideConfig (view nodeDerivation <$> stackConfAllPackages)

        TargetStackagePackages -> do
          (buildPlanFile, buildPlan, packageSetConfig) <- loadStackageBuildPlan hackageDb stackResolver opts sopts
          let
            overrideConfig = mkOverrideConfig opts (siGhcVersion $ bpSystemInfo buildPlan)
            stackageSetConfig = if opts ^. optTweaks . tExtraDepsRevisionLatest
              then extraDepsLatestConfig stackConfAllPackages packageSetConfig
              else packageSetConfig
            -- stackageLoader mHash pkgId =
            --   if pkgName pkgId `Set.member` reachable
            --   then packageLoader packageSetConfig Nothing pkgId
            --   else packageLoader packageSetConfig mHash pkgId
            -- stackageSetConfig = packageSetConfig { packageLoader = stackageLoader }
          nodes <- buildStackagePackages stackageSetConfig buildPlan
          writeOutFile buildPlanFile (opts ^. optOutStackagePackages)
            $ pPrintOutPackages (view nodeDerivation <$> nodes)
          writeOutFile buildPlanFile (opts ^. optOutStackageConfig)
            $ pPrintOutConfig (bpSystemInfo buildPlan) nodes
          writeOutFile (stackYaml ^. syFilePath) (opts ^. optOutDerivation)
            $ PP.overrideHaskellPackages overrideConfig (view nodeDerivation <$> stackConfAllPackages)

        TargetStackageOverride -> do
          writeOutFile (stackYaml ^. syFilePath) (opts ^. optOutDerivation)
            $ PP.overrideStackage
            (stackConf ^. scResolver)
            (opts ^. optNixpkgsRepository)
            (view nodeDerivation <$> stackConfAllPackages)
          writeOutFile (stackYaml ^. syFilePath) (opts ^. optOutStackYamlPackages)
            $ PP.stackYamlPackages (view nodeDerivation <$> stackConfPackages)

    -- Generate Stackage packages from resolver
    OriginResolver stackResolver -> do
      (buildPlanFile, buildPlan, packageSetConfig) <- loadStackageBuildPlan hackageDb stackResolver opts sopts
      nodes <- buildStackagePackages packageSetConfig buildPlan
      let overrideConfig = mkOverrideConfig opts (siGhcVersion $ bpSystemInfo buildPlan)
      writeOutFile buildPlanFile (opts ^. optOutStackagePackages)
        $ pPrintOutPackages (view nodeDerivation <$> nodes)
      writeOutFile buildPlanFile (opts ^. optOutStackageConfig)
        $ pPrintOutConfig (bpSystemInfo buildPlan) nodes
      writeOutFile buildPlanFile (opts ^. optOutDerivation)
        $ PP.pPrintHaskellPackages overrideConfig

loadStackageBuildPlan :: DB.HackageDB -> StackResolver -> Options -> StackageOptions -> IO (FilePath, BuildPlan, PackageSetConfig)
loadStackageBuildPlan hackageDb stackResolver opts sopts = do
  let buildPlanFile = LH.buildPlanFilePath (sopts ^. soptLtsHaskellRepo) stackResolver
  buildPlan <- LH.loadBuildPlan buildPlanFile
  packageSetConfig <- LH.buildPackageSetConfig
    hackageDb
    (sopts ^. soptAllCabalHashesRepo)
    (opts ^. optNixpkgsRepository)
    buildPlan
  return (buildPlanFile, buildPlan, packageSetConfig)

buildStackagePackages :: PackageSetConfig -> BuildPlan -> IO [Node]
buildStackagePackages packageSetConfig buildPlan = do
  let
    stackagePackageConfig = PackageConfig
      { enableCheck     = True
      , enableHaddock   = True }
  traverse (uncurry (buildNode packageSetConfig stackagePackageConfig))
  $ Map.toAscList (bpPackages buildPlan)

reachableDeps :: Traversable t => t Node -> Set.Set PackageName
reachableDeps = Set.map mkPackageName
  . F.foldr1 Set.union
  . fmap nodeDepends

-- Return 'PackageSetConfig' with package loader that loads latest Hackage
-- revison for direct (reachable) dependencies of input packages
extraDepsLatestConfig :: Traversable t => t Node -> PackageSetConfig -> PackageSetConfig
extraDepsLatestConfig packages packageSetConfig =
  let
    reachable = reachableDeps packages
    stackageLoader mHash pkgId =
      if pkgName pkgId `Set.member` reachable
      then packageLoader packageSetConfig Nothing pkgId
      else packageLoader packageSetConfig mHash pkgId
  in packageSetConfig { packageLoader = stackageLoader }

writeOutFile :: Show source => source -> FilePath -> Doc -> IO ()
writeOutFile source filePath contents =
  withFile filePath WriteMode $ \h -> do
    hPutStrLn h ("# Generated by stackage2nix " ++ Text.display version ++ " from " ++ show source)
    hPutStrLn h $ render contents

mkOverrideConfig :: Options -> Version -> OverrideConfig
mkOverrideConfig opts ghcVersion = OverrideConfig
  { _ocGhc              = ghcVersion
  , _ocStackagePackages = opts ^. optOutStackagePackages
  , _ocStackageConfig   = opts ^. optOutStackageConfig
  , _ocNixpkgs          = opts ^. optNixpkgsRepository }

mkStackPackagesConfig :: Options -> StackPackagesConfig
mkStackPackagesConfig opts = StackPackagesConfig
  { _spcHaskellResolver   = const True
  , _spcNixpkgsResolver   = \i -> Just (Nix.binding # (i, Nix.path # [i]))
  , _spcTargetPlatform    = opts ^. optPlatform
  , _spcTargetCompiler    = unknownCompilerInfo (opts ^. optCompilerId) NoAbiTag
  , _spcFlagAssignment    = mempty
  , _spcDoCheckPackages   = opts ^. optDoCheckPackages
  , _spcDoHaddockPackages = opts ^. optDoHaddockPackages }
