module Runner ( run ) where

import Control.Lens
import Distribution.Nixpkgs.Haskell.Stack
import Distribution.Nixpkgs.Haskell.Stack.PrettyPrinting as PP
import Options.Applicative
import Runner.Cli
import Stack.Config

import qualified Stackage2nix as S2n


run :: IO ()
run = do
  opts <- execParser pinfo
  stackYaml <- either fail pure =<< readStackConfig (opts ^. optStackYaml)

  -- generate stackage packages
  let
    s2nOptions = S2n.Options
      { S2n.optBuildPlanFile     = opts ^. optStackageBuildPlan
      , S2n.optAllCabalHashes    = opts ^. optAllCabalHashes
      , S2n.optNixpkgsRepository = opts ^. optNixpkgsRepository
      , S2n.optNixpkgsMap        = Nothing
      , S2n.optOutPackages       = opts ^. optStackageOutPackages
      , S2n.optOutConfig         = opts ^. optStackageOutConfig }
  _ <- S2n.run s2nOptions

  -- generate haskell packages override
  let
    pkgConfig = mkPackageConfig (opts ^. optPlatform) (opts ^. optCompilerId)
    overrideConfig = mkOverrideConfig opts
  packages <- traverse (packageDerivation pkgConfig (opts ^. optHackageDb))
    $ stackYaml ^. scPackages
  let
    out = PP.overrideHaskellPackages overrideConfig packages

  writeFile (opts ^. optOutFile) (show out)
  putStrLn $ "\nDerivation was written to " ++ opts ^. optOutFile

mkOverrideConfig :: Options -> OverrideConfig
mkOverrideConfig opts = OverrideConfig
  { _ocGhc              = "ghc802"
  , _ocStackagePackages = opts ^. optStackageOutPackages
  , _ocStackageConfig   = opts ^. optStackageOutConfig }
