{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Distribution.Nixpkgs.Haskell.Stack where

import Control.Lens
import Data.Text as T
import Distribution.System as System
import Distribution.Compiler as Compiler
import Distribution.PackageDescription as PackageDescription
import Distribution.Nixpkgs.Fetch
import Distribution.Nixpkgs.Haskell.Derivation
import Distribution.Nixpkgs.Haskell.FromCabal as FromCabal
import Distribution.Nixpkgs.Haskell.PackageSourceSpec as PackageSourceSpec
import Language.Nix
import Stack.Config

newtype HackageDb = HackageDb { fromHackageDB :: T.Text }
  deriving (Eq, Ord, Show)

makePrisms ''HackageDb

unHackageDb :: HackageDb -> String
unHackageDb = T.unpack . fromHackageDB

data PackageConfig = PackageConfig
  { _pcHaskellResolver :: HaskellResolver
  , _pcNixpkgsResolver :: NixpkgsResolver
  , _pcTargetPlatform  :: Platform
  , _pcTargetCompiler  :: CompilerInfo
  , _pcFlagAssignment  :: FlagAssignment
  }

makeLenses ''PackageConfig

mkPackageConfig :: Platform -> CompilerId -> FlagAssignment -> PackageConfig
mkPackageConfig platform compilerId flagAssignment = PackageConfig
  { _pcHaskellResolver = const True
  , _pcNixpkgsResolver = \i -> Just (binding # (i, path # [i]))
  , _pcTargetPlatform  = platform
  , _pcTargetCompiler  = unknownCompilerInfo compilerId NoAbiTag
  , _pcFlagAssignment  = flagAssignment
  }

-- Thin wrapper around private stack2nix 'PackageSourceSpec.fromDB' method.
getStackPackageFromDb
  :: Maybe HackageDb
  -> StackPackage
  -> IO Package
getStackPackageFromDb optHackageDb stackPackage =
  PackageSourceSpec.getPackage
    (unHackageDb <$> optHackageDb)
    (stackLocationToSource $ stackPackage ^. spSource)

stackLocationToSource :: StackLocation -> Source
stackLocationToSource = \case
  HackagePackage p -> Source
    { sourceUrl      = "cabal://" ++ T.unpack p
    , sourceRevision = mempty
    , sourceHash     = UnknownHash }
  StackPath p     -> Source
    { sourceUrl      = T.unpack p
    , sourceRevision = mempty
    , sourceHash     = UnknownHash }
  StackRepoGit rg -> Source
    { sourceUrl      = T.unpack $ rg ^. rgUri
    , sourceRevision = T.unpack $ rg ^. rgCommit
    , sourceHash     = UnknownHash }

packageDerivation
  :: PackageConfig
  -> Maybe HackageDb
  -> StackPackage
  -> IO Derivation
packageDerivation conf optHackageDb stackPackage = do
  pkg <- getStackPackageFromDb optHackageDb stackPackage
  pure $ genericPackageDerivation conf pkg
    & src .~ pkgSource pkg

genericPackageDerivation
  :: PackageConfig
  -> Package
  -> Derivation
genericPackageDerivation conf pkg =
  FromCabal.fromGenericPackageDescription
    (conf ^. pcHaskellResolver)
    (conf ^. pcNixpkgsResolver)
    (conf ^. pcTargetPlatform)
    (conf ^. pcTargetCompiler)
    (conf ^. pcFlagAssignment)
    []
    (pkgCabal pkg)
