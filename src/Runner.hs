{-# LANGUAGE RecordWildCards #-}

module Runner where

import Cli
import Control.Lens
import Distribution.Nixpkgs.Haskell.Stack
import Distribution.Nixpkgs.Haskell.Stack.PrettyPrinting as PP
import Distribution.PackageDescription ( FlagName(..), FlagAssignment )
import Distribution.Simple.Utils ( lowercase )
import Language.Nix.PrettyPrinting as PP
import Options.Applicative
import Stack.Config
import Stack.Types


readFlagList :: [CabalFlag] -> FlagAssignment
readFlagList = fmap (tagWithValue . unCabalFlag)
  where tagWithValue ('-':fname) = (FlagName (lowercase fname), False)
        tagWithValue fname       = (FlagName (lowercase fname), True)

run :: IO ()
run = do
  Options{..} <- execParser pinfo
  stackYaml <- either fail pure =<< readStackConfig optStackYaml
  derivations <- traverse (extraDepDerivation optHackageDb optPlatform optCompilerId (readFlagList optFlags))
    $ stackYaml ^. scExtraDeps
  let out = PP.overrideHaskellPackages derivations (PP.text "ghc802")
  print out
  --print $ nixpkgsOverride overrideConfigFixture
