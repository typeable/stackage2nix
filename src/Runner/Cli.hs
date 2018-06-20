module Runner.Cli where

import           Control.Lens
import           Data.Monoid ( (<>) )
import           Data.Text as T
import qualified Distribution.Compat.ReadP as P
import           Distribution.Compiler as Compiler
import           Distribution.System as System
import qualified Distribution.Text as Text
import           Options.Applicative as Opts
import           Paths_stackage2nix ( version )
import           Stack.Config
import           Stack.Types
import           System.FilePath

data Tweaks = Tweaks
  { _tExtraDepsRevisionLatest :: Bool
  } deriving (Show)

makeLenses ''Tweaks

-- | What will be generated
data Target
  = TargetStackagePackages
  | TargetStackageClosure
  | TargetStackageOverride
  deriving (Show)

makePrisms ''Target

-- | Source that would be used to produce target derivation
data ConfigOrigin
  = OriginResolver StackResolver
  -- ^ Resolver to generate Stackage LTS
  | OriginStackYaml StackYaml Target
  -- ^ Stack config to generate build derivation
  deriving (Show)

makePrisms ''ConfigOrigin

data StackageOptions = StackageOptions
  { _soptAllCabalHashesRepo :: FilePath
  , _soptLtsHaskellRepo     :: FilePath
  } deriving (Show)

makeLenses ''StackageOptions

data Options = Options
  { _optAllCabalHashesRepo   :: Maybe FilePath
  , _optLtsHaskellRepo       :: Maybe FilePath
  , _optOutStackagePackages  :: FilePath
  , _optOutStackageConfig    :: FilePath
  , _optOutDerivation        :: FilePath
  , _optOutStackYamlPackages :: FilePath
  , _optDoCheckPackages      :: Bool
  , _optDoHaddockPackages    :: Bool
  , _optTweaks               :: Tweaks
  , _optHackageDb            :: Maybe FilePath
  , _optNixpkgsRepository    :: FilePath
  , _optCompilerId           :: CompilerId
  , _optPlatform             :: Platform
  , _optConfigOrigin         :: ConfigOrigin
  } deriving (Show)

makeLenses ''Options

mkStackYaml :: FilePath -> StackYaml
mkStackYaml p = case splitFileName p of
  (dir, "")   -> StackYaml dir "stack.yaml"
  (dir, ".")  -> StackYaml dir "stack.yaml"
  (_  , "..") -> StackYaml p "stack.yaml"
  (dir, file) -> case splitExtension file of
    (_, "") -> StackYaml p "stack.yaml"
    _       -> StackYaml dir file

options :: Parser Options
options = Options
  <$> optional allCabalHashesRepo
  <*> optional ltsHaskellRepo
  <*> outStackagePackages
  <*> outStackageConfig
  <*> outDerivation
  <*> outStackYamlPackages
  <*> doCheckPackages
  <*> doHaddockPackages
  <*> tweaks
  <*> optional optionHackageDb
  <*> nixpkgsRepository
  <*> compilerId
  <*> platform
  <*> configOrigin

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

outDerivation :: Parser FilePath
outDerivation = option str
  ( long "out-derivation"
    <> metavar "NIX_FILE"
    <> help "path to output derivation"
    <> value "default.nix"
    <> showDefaultWith id )

outStackYamlPackages :: Parser FilePath
outStackYamlPackages = option str
  ( long "out-stack-yaml-packages"
    <> metavar "NIX_FILE"
    <> help "path to output stackage.yaml packages list"
    <> value "stack-yaml-packages.nix"
    <> showDefaultWith id )

doCheckPackages :: Parser Bool
doCheckPackages = flag True False
  ( long "no-check"
    <> help "disable tests for project packages")

doHaddockPackages :: Parser Bool
doHaddockPackages = flag True False
  ( long "no-haddock"
    <> help "disable haddock for project packages")

withStackage :: Parser ()
withStackage = flag' ()
  ( long "with-stackage"
    <> help "generate full Stackage" )

withStackageClosure :: Parser ()
withStackageClosure = flag' ()
  ( long "with-stackage-closure"
    <> help "generate Stackage subset containing only build dependencies" )

extraDepsRevisionLatest :: Parser Bool
extraDepsRevisionLatest = switch
  ( long "extra-deps-revision-latest"
    <> help "Changes algorithm for the dependencies generation of the stack.yaml extra-deps. Default one is to generate exact revision defined in the Stackage config. With this flag enabled, direct dependencies of the stack.yaml extra-deps will be generated with the latest revision available on Hackage. This may help when Cabal fails to resolve dependencies of extra-deps, and dependences are fixed in the latest revision" )

tweaks :: Parser Tweaks
tweaks = Tweaks <$> extraDepsRevisionLatest

resolver :: Parser StackResolver
resolver = StackResolver <$> option text
  ( long "resolver"
    <> help "stackage LTS resolver" )

stackYamlArg :: Parser StackYaml
stackYamlArg = mkStackYaml <$> Opts.argument str
  ( metavar "STACK_YAML"
    <> help "path to stack.yaml file or directory" )

target :: Parser Target
target =
  withStackage *> pure TargetStackagePackages <|>
  withStackageClosure *> pure TargetStackageClosure <|>
  pure TargetStackageOverride

configOrigin :: Parser ConfigOrigin
configOrigin =
  OriginStackYaml <$> stackYamlArg <*> target <|>
  OriginResolver <$> resolver

-- required for cabal2nix

optionHackageDb :: Parser FilePath
optionHackageDb = option str
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
