module Cli where

import           Data.Monoid ( (<>) )
import           Data.Text as T
import qualified Distribution.Compat.ReadP as P
import           Distribution.Compiler as Compiler
import           Distribution.Nixpkgs.Haskell.Stack
import           Distribution.System as System
import qualified Distribution.Text as Text
import           Options.Applicative
import           Stack.Config
import           Stack.Types


data Options = Options
  { optStackResolver :: Maybe StackResolver
  , optHackageDb     :: Maybe HackageDb
  , optFlags         :: [CabalFlag]
  , optCompilerId    :: CompilerId
  , optPlatform      :: Platform
  , optStackYaml     :: FilePath
  } deriving (Show)

options :: Parser Options
options = Options
  <$> optional stackResolver
  <*> optional hackageDb
  <*> many cabalFlag
  <*> compilerId
  <*> platform
  <*> stackYamlArg

pinfo :: ParserInfo Options
pinfo = info
  (   helper
  <*> options )
  (  fullDesc
  <> header "stack2nix converts Stack files into build instructions for Nix." )

stackResolver :: Parser StackResolver
stackResolver = StackResolver <$>
  option text
    ( long "resolver"
      <> metavar "RESOLVER"
      <> help "override stack resolver" )

stackYamlArg :: Parser FilePath
stackYamlArg = argument str
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
