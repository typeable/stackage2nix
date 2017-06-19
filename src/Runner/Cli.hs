{-# LANGUAGE TemplateHaskell #-}

module Runner.Cli where

import           Control.Lens
import           Data.Monoid ( (<>) )
import           Data.Text as T
import qualified Distribution.Compat.ReadP as P
import           Distribution.Compiler as Compiler
import           Distribution.Nixpkgs.Haskell.Stack
import           Distribution.System as System
import qualified Distribution.Text as Text
import           Options.Applicative as Opts
import           Stack.Config
import           Stack.Types


data Options = Options
  { _optOutFile             :: !FilePath
  , _optHackageDb           :: !(Maybe HackageDb)
  , _optNixpkgsRepository   :: !FilePath
  , _optAllCabalHashesPath  :: !FilePath
  , _optLtsHaskellPath      :: !FilePath
  , _optStackageOutPackages :: !FilePath
  , _optStackageOutConfig   :: !FilePath
  , _optCompilerId          :: !CompilerId
  , _optPlatform            :: !Platform
  , _optStackYaml           :: !FilePath
  } deriving (Show)

makeLenses ''Options

options :: Parser Options
options = Options
  <$> outFile
  <*> optional hackageDb
  <*> nixpkgsRepository
  <*> allCabalHashesPath
  <*> ltsHaskellPath
  <*> stackageOutPackages
  <*> stackageOutConfig
  <*> compilerId
  <*> platform
  <*> stackYamlArg

pinfo :: ParserInfo Options
pinfo = info
  (   helper
  <*> options )
  (  fullDesc
  <> header "stack2nix converts Stack files into build instructions for Nix." )

outFile :: Parser FilePath
outFile = option str
  ( long "nix-out-file"
    <> metavar "NIX_OUT_FILE"
    <> help "path to output derivation"
    <> value "default.nix"
    <> showDefaultWith id )

nixpkgsRepository :: Parser FilePath
nixpkgsRepository = option str
  ( long "nixpkgs"
    <> metavar "NIXPKGS"
    <> help "path to Nixpkgs repository"
    <> value "<nixpkgs>"
    <> showDefaultWith id )

stackageBuildPlan :: Parser FilePath
stackageBuildPlan = option str
  ( long "stackage-build-plan"
    <> metavar "STACKAGE_BUILD_PLAN"
    <> help "stackage build plan (YAML)" )

allCabalHashesPath :: Parser FilePath
allCabalHashesPath = option str
  ( long "all-cabal-hashes"
    <> metavar "ALL_CABAL_HASHES"
    <> help "path to commercialhaskell/all-cabal-hashes repository" )

ltsHaskellPath :: Parser FilePath
ltsHaskellPath = option str
  ( long "lts-haskell"
    <> metavar "LTS_HASKELL_DIR"
    <> help "path to fpco/lts-haskell repository" )

stackResolver :: Parser StackResolver
stackResolver = StackResolver <$>
  option text
    ( long "resolver"
      <> metavar "RESOLVER"
      <> help "override stack resolver" )

stackageOutPackages :: Parser FilePath
stackageOutPackages = option str
  ( long "nix-stackage-packages"
    <> metavar "STACKAGE_PACKAGES"
    <> help "name of the output file for the package set"
    <> value "packages.nix"
    <> showDefaultWith id)

stackageOutConfig :: Parser FilePath
stackageOutConfig = option str
  ( long "nix-stackage-config"
    <> metavar "STACKAGE_CONFIG"
    <> help "name of the output file for the package set configuration"
    <> value "configuration-packages.nix"
    <> showDefaultWith id)

stackYamlArg :: Parser FilePath
stackYamlArg = Opts.argument str
  ( metavar "STACK_YAML"
    <> value "stack.yaml" )

hackageDb :: Parser HackageDb
hackageDb = HackageDb <$>
  option text
    ( long "hackage-db"
      <> metavar "HACKAGE_DB"
      <> help "path to the local hackage db in tar format" )

cabalFlag :: Parser CabalFlag
cabalFlag = CabalFlag <$>
  option text
    ( short 'f'
      <> long "flag"
      <> help "Cabal flag (may be specified multiple times)" )

compilerId :: Parser CompilerId
compilerId = option (readP Text.parse)
  ( long "compiler"
    <> help "compiler to use when evaluating the Cabal file"
    <> value buildCompilerId
    <> showDefaultWith Text.display )

platform :: Parser Platform
platform = option (readP Text.parse)
  ( long "system"
    <> help "target system to use when evaluating the Cabal file"
    <> value buildPlatform
    <> showDefaultWith Text.display )

text :: ReadM Text
text = T.pack <$> str

readP :: P.ReadP a a -> ReadM a
readP p = eitherReader $ \s ->
  case [ r' | (r',"") <- P.readP_to_S p s ] of
    (r:_) -> Right r
    _     -> Left ("invalid value " ++ show s)
