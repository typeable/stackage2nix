{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stackage2nix where

import AllCabalHashes
import Data.Monoid
import Options.Applicative
import Stackage.BuildPlan
import Stackage.Types (SystemInfo(..))
import Paths_stack2nix as Main
import Distribution.Text (display)
import Control.Monad
import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Distribution.Compiler (CompilerInfo(..), AbiTag(NoAbiTag), CompilerId(..), CompilerFlavor(GHC))
import Distribution.System (Platform(..))
import Distribution.Package (PackageName, PackageIdentifier(..), Dependency(..))
import Distribution.Nixpkgs.Haskell.PackageSourceSpec
import Distribution.Nixpkgs.Haskell.FromStack
import Distribution.Nixpkgs.Haskell.FromStack.Package
import Distribution.Nixpkgs.Fetch
import Distribution.Nixpkgs.PackageMap (readNixpkgPackageMap, resolve, PackageMap)
import Distribution.Version (Version, withinRange)
import Language.Haskell.Extension (Language(Haskell98, Haskell2010))
import Language.Nix
import Control.Lens
import Data.Maybe
import System.IO (withFile, IOMode(..), hPutStrLn)

import qualified Data.Aeson as Aeson
import qualified Data.Yaml as Yaml
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap


data Options = Options
  { optBuildPlanFile :: FilePath
  , optAllCabalHashes :: FilePath
  , optDoCheck :: Bool
  , optDoHaddock :: Bool
  , optNixpkgsRepository :: FilePath
  , optNixpkgsMap :: Maybe FilePath
  , optOutPackages :: FilePath
  , optOutConfig :: FilePath
  }

options :: Parser Options
options = Options
  <$> strArgument (metavar "PLAN" <> help "stackage build plan (YAML)")
  <*> strArgument (metavar "CABALFILES" <> help "path to checkout of all-cabal-hashes")
  <*> switch (long "do-check" <> help "enable tests for Stackage packages")
  <*> switch (long "do-haddock" <> help "enable haddock for Stackage packages")
  <*> strOption (long "nixpkgs" <> help "path to Nixpkgs repository" <> value "<nixpkgs>" <> showDefaultWith id <> metavar "PATH")
  <*> optional (strOption (long "package-map" <> help "path to a serialized nixpkgs package map" <> metavar "PATH"))
  <*> strOption (long "out-packages" <> help "name of the output file for the package set" <> value "packages.nix" <> metavar "PATH")
  <*> strOption (long "out-config" <> help "name of the output file for the package set configuration" <> value "configuration-packages.nix" <> metavar "PATH")

pinfo :: ParserInfo Options
pinfo = info
        (   helper
        <*> infoOption ("stackage2nix " ++ display Main.version) (long "version" <> help "Show version number")
        <*> options
        )
        (  fullDesc
        <> header "stackage2nix converts stackage snapshots into a nix package set"
        )

loadBuildPlan :: FilePath -> IO BuildPlan
loadBuildPlan = Yaml.decodeFileEither >=> \case
  Left err -> fail $ "Failed to parse stackage build plan: " ++ show err
  Right bp -> return bp

loadPackage :: FilePath -> Maybe SHA1Hash -> PackageIdentifier -> IO Package
loadPackage allCabalHashesPath mSha1Hash pkgId = do
  (pkgDesc, _) <- case mSha1Hash of
    Just sha1 -> readPackageByHash allCabalHashesPath sha1
    Nothing   -> readPackageByName allCabalHashesPath pkgId
  meta <- readPackageMeta allCabalHashesPath pkgId
  let
    tarballSHA256 = fromMaybe
      (error (display pkgId ++ ": meta data has no SHA256 hash for the tarball"))
      (view (mHashes . at "SHA256") meta)
    source = DerivationSource "url" ("mirror://hackage/" ++ display pkgId ++ ".tar.gz") "" tarballSHA256
  return $ Package source pkgDesc

ghcCompilerInfo :: Version -> CompilerInfo
ghcCompilerInfo v = CompilerInfo
  { compilerInfoId = CompilerId GHC v
  , compilerInfoAbiTag = NoAbiTag
  , compilerInfoCompat = Just []
  , compilerInfoLanguages = Just [Haskell98, Haskell2010]
  , compilerInfoExtensions = Nothing
  }

buildPlanContainsDependency :: Map.Map PackageName Version -> Dependency -> Bool
buildPlanContainsDependency packageVersions (Dependency depName versionRange) =
  maybe False (`withinRange` versionRange) $ Map.lookup depName packageVersions

newtype SerializablePackageMap = SerializablePackageMap { getPackageMap :: PackageMap }

instance Aeson.ToJSON SerializablePackageMap where
  toJSON (SerializablePackageMap m) = Aeson.object . map makeEntry $ Map.toList m
   where
    makeEntry (i, s) = (Text.pack (view ident i), Aeson.toJSON . map makePath $ Set.toList s)
    makePath = view (path . mapping ident)

instance Aeson.FromJSON SerializablePackageMap where
  parseJSON = Aeson.withObject "package map object" $
    fmap (SerializablePackageMap . Map.fromList) . traverse makeEntry . HashMap.toList
   where
    makeEntry (i, s) = Aeson.parseJSON s <&> \s' -> (ident # Text.unpack i, Set.fromList (map makePath s'))
    makePath = review (path . mapping ident)

run :: Options -> IO (BuildPlan, PackageSetConfig)
run Options{..} = do
  nixpkgs <- fromMaybe (readNixpkgPackageMap optNixpkgsRepository Nothing) $ do
    mapFile <- optNixpkgsMap
    return $ do
      contents <- ByteString.readFile mapFile
      case Aeson.eitherDecodeStrict contents of
        Left err -> fail $ "Failed to decode package map " ++ show mapFile ++ ": " ++ err
        Right v -> return $ getPackageMap v

  buildPlan <- loadBuildPlan optBuildPlanFile

  let
    systemInfo = bpSystemInfo buildPlan
    packageVersions = fmap ppVersion (bpPackages buildPlan) `Map.union` siCorePackages systemInfo
    conf = PackageSetConfig
      { packageLoader = loadPackage optAllCabalHashes
      , targetPlatform = Platform (siArch systemInfo) (siOS systemInfo)
      , targetCompiler = ghcCompilerInfo (siGhcVersion systemInfo)
      , nixpkgsResolver = resolve (Map.map (Set.map (over path ("pkgs":))) nixpkgs)
      , haskellResolver = buildPlanContainsDependency packageVersions
      , enableCheck = optDoCheck
      , enableHaddock = optDoHaddock
      }
  return (buildPlan, conf)

writeOutPackages :: FilePath -> FilePath -> [Node] -> IO ()
writeOutPackages optBuildPlanFile optOutPackages nodes =
  withFile optOutPackages WriteMode $ \h -> do
    hPutStrLn h ("# Generated by stackage2nix from " ++ optBuildPlanFile)
    hPutStrLn h $ render $ pPrintOutPackages (view nodeDerivation <$> nodes)

writeOutConfig :: FilePath -> FilePath -> SystemInfo -> [Node] -> IO ()
writeOutConfig optBuildPlanFile optOutConfig systemInfo nodes =
  withFile optOutConfig WriteMode $ \h -> do
    hPutStrLn h ("# Generated by stackage2nix from " ++ optBuildPlanFile)
    hPutStrLn h $ render $ pPrintOutConfig systemInfo nodes

main :: IO ()
main = do
  opts@Options{..} <- execParser pinfo
  (buildPlan, conf) <- run opts
  let systemInfo = bpSystemInfo buildPlan
  nodes <- traverse (uncurry $ buildNodeM conf) $ Map.toList (bpPackages buildPlan)
  withFile optOutPackages WriteMode $ \h -> do
    hPutStrLn h ("# Generated by stackage2nix from " ++ optBuildPlanFile)
    hPutStrLn h $ render $ pPrintOutPackages (view nodeDerivation <$> nodes)
  withFile optOutConfig WriteMode $ \h -> do
    hPutStrLn h ("# Generated by stackage2nix from " ++ optBuildPlanFile)
    hPutStrLn h $ render $ pPrintOutConfig systemInfo nodes
