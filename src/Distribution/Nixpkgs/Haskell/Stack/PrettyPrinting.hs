{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Distribution.Nixpkgs.Haskell.Stack.PrettyPrinting where

import           Control.Lens
import           Data.Maybe
import           Data.String
import           Distribution.Nixpkgs.Haskell.Derivation
import           Distribution.Nixpkgs.Haskell.Packages.PrettyPrinting as PP
import           Distribution.Package
import           Distribution.Text
import qualified Language.Nix.FilePath as Nix
import           Language.Nix.PrettyPrinting as PP

data OverrideConfig = OverrideConfig
  { _ocGhc :: String
  , _ocStackagePackages :: Maybe FilePath
  , _ocStackageConfig :: Maybe FilePath
  }

makeLenses ''OverrideConfig

hasField :: Lens' OverrideConfig (Maybe a) -> OverrideConfig -> Bool
hasField p = views p isJust

overridePackages :: [Derivation] -> Doc
overridePackages deps = PP.packageSetConfig . PP.cat $ callPackage <$> deps
  where
    drvNameString = PP.doubleQuotes . disp . pkgName . view pkgid
    callPackage drv = hang
      (drvNameString drv <> " = callPackage") 2
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

overrideHaskellPackages :: OverrideConfig -> [Derivation] -> Doc
overrideHaskellPackages oc packages = vcat
  [ funargs ["nixpkgs ? import <nixpkgs> {}"]
  , ""
  , "with nixpkgs;"
  , "let"
  , nest 2 "inherit (stdenv.lib) extends;"
  , nest 2 . vcat $ catMaybes
    [ attr "stackagePackages" . importStackagePackages <$> oc ^. ocStackagePackages
    , attr "stackageConfig" . callStackageConfig <$> oc ^. ocStackageConfig ]
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
  , "in callPackage <nixpkgs/pkgs/development/haskell-modules> {"
  , nest 2 $ vcat
    [ attr "ghc" ("pkgs.haskell.compiler." <> ghc)
    , onlyIf (hasField ocStackageConfig oc && hasField ocStackagePackages oc)
      $ attr "compilerConfig" "self: extends stackageConfig (stackagePackages self)"
    , attr "overrides" "self: pkgOverrides self"]
  , "}"
  ]
  where
    ghc = oc ^. ocGhc . to text
