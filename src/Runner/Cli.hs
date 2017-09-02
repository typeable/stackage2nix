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
import           Paths_stackage2nix ( version )
import           Stack.Config
import           Stack.Types
import           System.Environment
import           System.FilePath

-- | Which source to use
data Target
  = TargetResolver StackResolver
  -- ^ Resolver to generate Stackage LTS
  | TargetStackYaml StackYaml
  -- ^ Stack config to generate build derivation
  deriving (Show)

makePrisms ''Target

data Options = Options
  { _optAllCabalHashesRepo  :: !FilePath
  , _optLtsHaskellRepo      :: !FilePath
  , _optOutStackagePackages :: !FilePath
  , _optOutStackageConfig   :: !FilePath
  , _optOutPackagesClosure  :: !Bool
  , _optOutDerivation       :: !FilePath
  , _optDoCheckPackages     :: !Bool
  , _optDoHaddockPackages   :: !Bool
  , _optDoCheckStackage     :: !Bool
  , _optDoHaddockStackage   :: !Bool
  , _optHackageDb           :: !(Maybe HackageDb)
  , _optNixpkgsRepository   :: !FilePath
  , _optCompilerId          :: !CompilerId
  , _optPlatform            :: !Platform
  , _optTarget              :: !Target
  } deriving (Show)

makeLenses ''Options

envStackYaml :: IO (Maybe StackYaml)
envStackYaml = fmap mkStackYaml <$> lookupEnv "STACK_YAML"

mkStackYaml :: FilePath -> StackYaml
mkStackYaml p = case splitFileName p of
  (dir, "")   -> StackYaml dir "stack.yaml"
  (dir, ".")  -> StackYaml dir "stack.yaml"
  (_  , "..") -> StackYaml p "stack.yaml"
  (dir, file) -> StackYaml dir file

options :: Parser Options
options = Options
  <$> allCabalHashesRepo
  <*> ltsHaskellRepo
  <*> outStackagePackages
  <*> outStackageConfig
  <*> outPackagesClosure
  <*> outDerivation
  <*> doCheckPackages
  <*> doHaddockPackages
  <*> doCheckStackage
  <*> doHaddockStackage
  <*> optional hackageDb
  <*> nixpkgsRepository
  <*> compilerId
  <*> platform
  <*> target

pinfo :: ParserInfo Options
pinfo = info
  ( helper
    <*> infoOption ("stackage2nix " ++ Text.display version) (long "version" <> help "Show version")
    <*> options )
  ( fullDesc
  <> header "stackage2nix converts Stack files into build instructions for Nix." )

nixpkgsRepository :: Parser FilePath
nixpkgsRepository = option str
  ( long "nixpkgs"
    <> metavar "NIXPKGS"
    <> help "path to Nixpkgs repository"
    <> value "<nixpkgs>"
    <> showDefaultWith id )

allCabalHashesRepo :: Parser FilePath
allCabalHashesRepo = option str
  ( long "all-cabal-hashes"
    <> metavar "REPO_DIR"
    <> help "path to commercialhaskell/all-cabal-hashes repository" )

ltsHaskellRepo :: Parser FilePath
ltsHaskellRepo = option str
  ( long "lts-haskell"
    <> metavar "REPO_DIR"
    <> help "path to fpco/lts-haskell repository" )

outStackagePackages :: Parser FilePath
outStackagePackages = option str
  ( long "out-packages"
    <> metavar "NIX_FILE"
    <> help "output file of the stackage packages set"
    <> value "packages.nix"
    <> showDefaultWith id)

outStackageConfig :: Parser FilePath
outStackageConfig = option str
  ( long "out-config"
    <> metavar "NIX_FILE"
    <> help "output file of the stackage packages compiler config"
    <> value "configuration-packages.nix"
    <> showDefaultWith id)

outPackagesClosure :: Parser Bool
outPackagesClosure = flag True False
  ( long "no-packages-closure"
    <> help "produce full set of stackage packages, not just dependencies" )

outDerivation :: Parser FilePath
outDerivation = option str
  ( long "out-derivation"
    <> metavar "NIX_FILE"
    <> help "path to output derivation"
    <> value "default.nix"
    <> showDefaultWith id )

doCheckPackages :: Parser Bool
doCheckPackages = flag True False
  ( long "no-check"
    <> help "disable tests for project packages")

doHaddockPackages :: Parser Bool
doHaddockPackages = flag True False
  ( long "no-haddock"
    <> help "disable haddock for project packages")

doCheckStackage :: Parser Bool
doCheckStackage = switch
  ( long "do-check-stackage"
    <> help "enable tests for Stackage packages" )

doHaddockStackage :: Parser Bool
doHaddockStackage = switch
  ( long "do-haddock-stackage"
    <> help "enable haddock for Stackage packages" )

resolver :: Parser StackResolver
resolver = StackResolver <$> option text
  ( long "resolver"
    <> help "stackage LTS resolver" )

stackYamlArg :: Parser StackYaml
stackYamlArg = mkStackYaml <$> Opts.argument str
  ( metavar "STACK_YAML"
    <> help "path to stack.yaml file or directory" )

target :: Parser Target
target = TargetResolver <$> resolver
  <|> TargetStackYaml <$> stackYamlArg

-- inherited from cabal2nix

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

-- utils

text :: ReadM Text
text = T.pack <$> str

readP :: P.ReadP a a -> ReadM a
readP p = eitherReader $ \s ->
  case [ r' | (r',"") <- P.readP_to_S p s ] of
    (r:_) -> Right r
    _     -> Left ("invalid value " ++ show s)
