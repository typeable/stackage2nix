{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Monoid ( (<>) )
import Options.Applicative
import Distribution.Nixpkgs.Haskell.Stack

data Options = Options
  { optStackYaml     :: Maybe FilePath
  , optStackResolver :: Maybe String
  , optOutDir        :: FilePath
  , optHackageDb     :: Maybe FilePath
  } deriving (Show)

options :: Parser Options
options = Options
  <$> optional (strArgument $ metavar "STACK_YAML")
  <*> optional (strOption $ long "resolver" <> metavar "RESOLVER" <> help "override stack resolver")
  <*> strOption (long "out" <> metavar "OUT" <> value "nix" <> showDefaultWith id <> help "output directory")
  <*> optional (strOption $ long "hackage-db" <> metavar "HACKAGE_DB" <> help "path to the local hackage db in tar format")

pinfo :: ParserInfo Options
pinfo = info
  (   helper
  <*> options )
  (  fullDesc
  <> header "stack2nix converts Stack files into build instructions for Nix." )


main :: IO ()
main = do
  Options {..} <- execParser pinfo
  print $ nixpkgsOverride overrideConfigFixture
