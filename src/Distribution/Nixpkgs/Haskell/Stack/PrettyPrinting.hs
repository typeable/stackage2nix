{-# LANGUAGE OverloadedStrings #-}

module Distribution.Nixpkgs.Haskell.Stack.PrettyPrinting where

import Control.Lens
import Distribution.Package
import Distribution.Text
import Distribution.Nixpkgs.Haskell.Derivation
import Distribution.Nixpkgs.Haskell.Packages.PrettyPrinting as PP
import Language.Nix.PrettyPrinting as PP


overrideExtraDeps :: [Derivation] -> Doc
overrideExtraDeps deps = PP.packageSetConfig . PP.cat $ callPackage <$> deps
  where
    drvNameString = PP.doubleQuotes . disp . pkgName . view pkgid
    callPackage drv = vcat
      [ drvNameString drv <> " = callPackage",
        nest 2 (PP.parens $ PP.pPrint drv) <+> "{};"]

overrideHaskellPackages :: [Derivation] -> Doc -> Doc
overrideHaskellPackages extraDeps ghc = vcat
  [ funargs ["nixpkgs ? import <nixpkgs> {}"]
  , ""
  , "with nixpkgs;"
  , "let"
  , nest 2 $ vcat
    [ "extraDepsPackages ="
    , overrideExtraDeps extraDeps <> semi
    , "pkgOverrides = self: extraDepsPackages {"
    , nest 2 "inherit pkgs stdenv;"
    , nest 2 "inherit (self) callPackage;"
    , "};"
    ]
  , "in pkgs.haskell.packages." <> ghc <> ".override {"
  , nest 2 $ vcat
    [ "overrides = self: pkgOverrides self;"]
  , "}"
  ]
