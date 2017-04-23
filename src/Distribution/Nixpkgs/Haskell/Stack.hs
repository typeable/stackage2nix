module Distribution.Nixpkgs.Haskell.Stack where

import Text.PrettyPrint

nixpkgsOverride :: Doc
nixpkgsOverride = vcat
  [ text "{ nixpkgs ? import <nixpkgs> {}, compiler ? \"default\" }: "
  , text ""
  , text "let"
  , text "  inherit (nixpkgs.stdenv.lib) extends;"
  , text "  packageOverrides = super: let self = super.pkgs; in"
  , text "  {"
  , text "    pkgs = super.pkgs // {"
  , text "      haskell = super.pkgs.haskell // {"
  , text "        packages = super.pkgs.haskell.packages // {"
  , text "          ${compiler} = super.pkgs.haskell.packages.ghc7103.override {};"
  , text "        };"
  , text "      };"
  , text "    };"
  , text "  };"
  , text "in packageOverrides nixpkgs"
  ]
