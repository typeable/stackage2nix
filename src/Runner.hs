module Runner ( run ) where

import Control.Lens
import Distribution.Nixpkgs.Haskell.Stack
import Distribution.Nixpkgs.Haskell.Stack.PrettyPrinting as PP
import Options.Applicative
import Runner.Cli
import Stack.Config


run :: IO ()
run = do
  opts <- execParser pinfo
  stackYaml <- either fail pure =<< readStackConfig (opts ^. optStackYaml)
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
  { _ocGhc = "ghc802"
  , _ocStackagePackages = unStackagePackages <$> opts ^. optStackagePackages
  , _ocStackageConfig = unStackageConfig <$> opts ^. optStackageConfig }
