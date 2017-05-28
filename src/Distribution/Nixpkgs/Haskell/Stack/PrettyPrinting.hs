{-# LANGUAGE OverloadedStrings #-}

module Distribution.Nixpkgs.Haskell.Stack.PrettyPrinting where

import Control.Lens
import Distribution.Package
import Distribution.Text
import Distribution.Nixpkgs.Haskell.Derivation
import Distribution.Nixpkgs.Haskell.Packages.PrettyPrinting as PP
import Language.Nix.PrettyPrinting as PP


overridePackages :: [Derivation] -> Doc
overridePackages deps = PP.packageSetConfig . PP.cat $ callPackage <$> deps
  where
    drvNameString = PP.doubleQuotes . disp . pkgName . view pkgid
    callPackage drv = hang
      (drvNameString drv <> " = callPackage") 2
      (PP.parens (PP.pPrint drv) <+> "{};")

overrideHaskellPackages :: Doc -> [Derivation] -> Doc
overrideHaskellPackages ghc packages = vcat
  [ funargs ["nixpkgs ? import <nixpkgs> {}"]
  , ""
  , "with nixpkgs;"
  , "let"
  , nest 2 $ vcat
    [ "packageSetConfig ="
    , nest 2 $ overridePackages packages <> semi
    , ""
    , "pkgOverrides = self: packageSetConfig {"
    , nest 2 "inherit pkgs stdenv;"
    , nest 2 "inherit (self) callPackage;"
    , "};"
    , ""
    ]
  , "in pkgs.haskell.packages." <> ghc <> ".override {"
  , nest 2 $ vcat
    [ "overrides = self: pkgOverrides self;"]
  , "}"
  ]
