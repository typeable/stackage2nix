module Runner where

import Control.Lens
import Distribution.Nixpkgs.Haskell.Stack
import Distribution.Nixpkgs.Haskell.Stack.PrettyPrinting as PP
import Language.Nix.PrettyPrinting as PP
import Options.Applicative
import Runner.Cli
import Stack.Config


run :: IO ()
run = do
  opts <- execParser pinfo
  stackYaml <- either fail pure =<< readStackConfig (opts ^. optStackYaml)
  let
    pkgConfig = mkPackageConfig (opts ^. optPlatform) (opts ^. optCompilerId)
  packages <- traverse (packageDerivation pkgConfig (opts ^. optHackageDb))
    $ stackYaml ^. scPackages
  let out = PP.overrideHaskellPackages (PP.text "ghc802") packages
  print (opts ^. optStackagePackages, opts ^. optStackageConfig)
