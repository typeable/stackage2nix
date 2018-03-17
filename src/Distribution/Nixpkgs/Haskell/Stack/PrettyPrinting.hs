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
import           Distribution.Version (Version)
import qualified Language.Nix.FilePath as Nix
import           Language.Nix.PrettyPrinting as PP
import           Stack.Config (StackResolver, unStackResolver)


data OverrideConfig = OverrideConfig
  { _ocGhc              :: Version
  , _ocStackagePackages :: FilePath
  , _ocStackageConfig   :: FilePath
  , _ocNixpkgs          :: FilePath
  }

makeLenses ''OverrideConfig

systemNixpkgs :: Doc
systemNixpkgs = "<nixpkgs>"

renderNixpkgs :: FilePath -> Doc
renderNixpkgs nixpkgsPath
  | fromString nixpkgsPath == systemNixpkgs = systemNixpkgs
  | otherwise = disp $ (fromString nixpkgsPath :: Nix.FilePath)

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
  ["import", disp (fromString path :: Nix.FilePath)]

importStackageConfig :: FilePath -> Doc
importStackageConfig path = hsep
  ["import ", disp (fromString path :: Nix.FilePath), "{ inherit pkgs haskellLib; }"]

overrideHaskellPackages :: OverrideConfig -> NonEmpty Derivation -> Doc
overrideHaskellPackages oc packages = vcat
  [ funargs
    [ "nixpkgs ? import " <> renderNixpkgs (oc ^. ocNixpkgs) <> " {}"
    ]
  , ""
  , "with nixpkgs;"
  , "let"
  , nest 2 "inherit (stdenv.lib) extends;"
  , nest 2 $ vcat
    [ attr "haskellLib" "callPackage (nixpkgs.path + \"/pkgs/development/haskell-modules/lib.nix\") {}"
    , attr "stackagePackages" . importStackagePackages $ oc ^. ocStackagePackages
    , attr "stackageConfig" . importStackageConfig $ oc ^. ocStackageConfig ]
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
  , "in callPackage (nixpkgs.path + \"/pkgs/development/haskell-modules\") {"
  , nest 2 $ vcat
    [ attr "ghc" ("pkgs.haskell.compiler." <> toNixGhcVersion (oc ^. ocGhc))
    , attr "compilerConfig" "self: extends pkgOverrides (stackageConfig self)"
    , attr "initialPackages" "stackagePackages"
    , attr "configurationCommon" "args: self: super: {}"
    , "inherit haskellLib;"
    ]
  , "}"
  ]

overrideStackage :: StackResolver -> FilePath -> NonEmpty Derivation -> Doc
overrideStackage stackResolver nixpkgsPath packages = vcat
  [ funargs
    [ "nixpkgs ? import " <> renderNixpkgs nixpkgsPath <> " {}"
    ]
  , ""
  , "let"
  , nest 2 $ vcat
    [ "stackPackages ="
    , nest 2 $ overridePackages packages <> semi
    ]
  , "in nixpkgs.haskell.packages.stackage." <> toNixStackResolver stackResolver <> ".override {"
  , nest 2
    $ attr "packageSetConfig" "self: super: stackPackages { inherit (nixpkgs) pkgs stdenv; inherit (self) callPackage; } super"
  , "}"
  ]

pPrintHaskellPackages :: OverrideConfig -> Doc
pPrintHaskellPackages oc = vcat
  [ funargs
    [ "callPackage"
    , "buildPackages"
    , "pkgs"
    , "stdenv"
    , "lib"
    , "overrides ? (self: super: {})"
    , "packageSetConfig ? (self: super: {})"
    ]
  , ""
  , "let"
  , nest 2 $ vcat
    [ "inherit (lib) extends makeExtensible;"
    , attr "haskellLib" "pkgs.haskell.lib"
    , "inherit (haskellLib) makePackageSet;"
    , ""
    , attr "haskellPackages" $ vcat
      [ "pkgs.callPackage makePackageSet {"
      , nest 2 $ vcat
        [ attr "ghc" ("buildPackages.haskell.compiler." <> toNixGhcVersion (oc ^. ocGhc))
        , attr "buildHaskellPackages" ("buildPackages.haskell.packages." <> toNixGhcVersion (oc ^. ocGhc))
        , attr "package-set" . importStackagePackages $ oc ^. ocStackagePackages
        , "inherit stdenv haskellLib extensible-self;"
        ]
      , "}"
      ]
    , ""
    , attr "compilerConfig" . importStackageConfig $ oc ^. ocStackageConfig
    , ""
    , attr "configurationCommon" "if builtins.pathExists ./configuration-common.nix then import ./configuration-common.nix { inherit pkgs haskellLib; } else self: super: {}"
    , attr "configurationNix" "import (pkgs.path + \"/pkgs/development/haskell-modules/configuration-nix.nix\") { inherit pkgs haskellLib; }"
    , ""
    , attr "extensible-self" "makeExtensible (extends overrides (extends configurationCommon (extends packageSetConfig (extends compilerConfig (extends configurationNix haskellPackages)))))"
    ]
  , ""
  , "in extensible-self"
  ]

toNixGhcVersion :: Version -> Doc
toNixGhcVersion = (<>) "ghc" . toNixVersion . show . disp

toNixStackResolver :: StackResolver -> Doc
toNixStackResolver = toNixVersion . unStackResolver

toNixVersion :: String -> Doc
toNixVersion = text . L.filter (/= '.')
