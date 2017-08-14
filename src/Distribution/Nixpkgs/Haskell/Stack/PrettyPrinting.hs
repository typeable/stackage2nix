{-# LANGUAGE Rank2Types #-}

module Distribution.Nixpkgs.Haskell.Stack.PrettyPrinting where

import           Control.Lens
import           Data.Foldable as F
import           Data.List as L
import           Data.List.NonEmpty as NE
import           Data.Maybe
import           Data.String
import           Distribution.Nixpkgs.Haskell.Derivation
import           Distribution.Nixpkgs.Haskell.Packages.PrettyPrinting as PP
import           Distribution.Package
import           Distribution.Text
import           Distribution.Version (Version(..))
import qualified Language.Nix.FilePath as Nix
import           Language.Nix.PrettyPrinting as PP


data OverrideConfig = OverrideConfig
  { _ocGhc              :: !Version
  , _ocStackagePackages :: !FilePath
  , _ocStackageConfig   :: !FilePath }

makeLenses ''OverrideConfig

hasField :: Lens' a (Maybe b) -> a -> Bool
hasField p = views p isJust

overridePackages :: (Foldable t, Functor t) => t Derivation -> Doc
overridePackages = PP.packageSetConfig . PP.cat . F.toList . fmap callPackage
  where
    drvNameQuoted   = PP.doubleQuotes . disp . pkgName . view pkgid
    callPackage drv = hang
      (drvNameQuoted drv <> " = callPackage") 2
      (PP.parens (PP.pPrint drv) <+> "{};")

importStackagePackages :: FilePath -> Doc
importStackagePackages path = hsep
  [ funarg "self",  "import", disp (fromString path :: Nix.FilePath), "{"
  , "inherit pkgs stdenv;"
  , "inherit (self) callPackage;"
  , "}"
  ]

importStackageConfig :: FilePath -> Doc
importStackageConfig path = hsep
  [ "import", disp (fromString path :: Nix.FilePath), "{"
  , "inherit pkgs;"
  , "}"
  ]

callStackageConfig :: FilePath -> Doc
callStackageConfig path = hsep
  [ "callPackage", disp (fromString path :: Nix.FilePath), "{}"]

overrideHaskellPackages :: OverrideConfig -> NonEmpty Derivation -> Doc
overrideHaskellPackages oc packages = vcat
  [ funargs ["nixpkgs ? import <nixpkgs> {}"]
  , ""
  , "with nixpkgs;"
  , "let nixpkgsPath = <nixpkgs>;"
  , nest 2 "inherit (stdenv.lib) extends;"
  , nest 2 ""
  , nest 2 $ vcat
    [ attr "stackagePackages" . importStackagePackages $ oc ^. ocStackagePackages
    , attr "stackageConfig" . callStackageConfig $ oc ^. ocStackageConfig ]
  , nest 2 $ vcat
    [ "stackPackages ="
    , nest 2 $ overridePackages packages <> semi
    , ""
    , "pkgOverrides = self: stackPackages {"
    , nest 2 "inherit pkgs stdenv;"
    , nest 2 "inherit (self) callPackage;"
    , "};"
    , ""
    ]
  , "in callPackage (nixpkgsPath + \"/pkgs/development/haskell-modules\") {"
  , nest 2 $ vcat
    [ attr "ghc" ("pkgs.haskell.compiler." <> toNixGhcVersion (oc ^. ocGhc))
    , attr "compilerConfig" "self: extends pkgOverrides (extends stackageConfig (stackagePackages self))"
    , "haskellLib = import (nixpkgsPath + \"/pkgs/development/haskell-modules/lib.nix\") {"
    , nest 2 "inherit pkgs;"
    , nest 2 "inherit (pkgs) lib;"
    , "};"
    , "inherit (stdenv) lib;"
    ]
  , "}"
  ]

toNixGhcVersion :: Version -> Doc
toNixGhcVersion =
  (<>) "ghc" . text . L.filter (/= '.') . show . disp
