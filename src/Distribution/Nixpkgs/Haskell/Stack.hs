{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Nixpkgs.Haskell.Stack where

import Language.Nix
import Language.Nix.PrettyPrinting

data StackConfig = StackConfig
  { stackageVersion :: String }

data OverrideConfig = OverrideConfig
  { stackageCompilerConfig :: FilePath
  -- ^ Nixpkgs compiler config for Stackage packages set
  , stackagePackagesConfig :: FilePath
  -- ^ Nixpkgs packages config for Stackage packages set
  , haskellPackagesOverride :: Identifier
  -- ^ 'pkgs.haskell.packages.<compiler>' to override
  }

overrideConfigFixture :: OverrideConfig
overrideConfigFixture = OverrideConfig
  { stackageCompilerConfig = "./haskell-modules/configuration-6_30.nix"
  , stackagePackagesConfig = "./haskell-modules/packages-6_30.nix"
  , haskellPackagesOverride = "ghc7103" }

stackageCompilerConfigFun :: FilePath -> Doc
stackageCompilerConfigFun p =
  funargs [text "superPkgs"] $$ "import" <+> string p
  <+> lbrace
  <+> text "inherit (superPkgs) pkgs;"
  <+> rbrace

nixLet :: [Doc] -> Doc
nixLet ls = vcat
  [ text "let"
  , nest 2 $ vcat ls
  ]

nixpkgsOverride :: OverrideConfig -> Doc
nixpkgsOverride OverrideConfig {..} = vcat
  [ text "{ nixpkgs ? import <nixpkgs> {}, compiler ? \"default\" }: "
  , text ""
  , nixLet
    [ text "inherit (nixpkgs.stdenv.lib) extends;"
    , attr "stackageCompilerConfig" $ stackageCompilerConfigFun stackageCompilerConfig
    , attr "stackagePackagesConfig" $ stackageCompilerConfigFun stackagePackagesConfig
    , text "packageOverrides = super: let self = super.pkgs; in"
    , text "{"
    , text "  pkgs = super.pkgs // {"
    , text "    haskell = super.pkgs.haskell // {"
    , text "      packages = super.pkgs.haskell.packages // {"
    , text "        \"${compiler}\" = super.pkgs.haskell.packages." <> disp haskellPackagesOverride <> ".override {};"
    , text "      };"
    , text "    };"
    , text "  };"
    , text "};"
    , text "in packageOverrides nixpkgs"
    ]
  ]
