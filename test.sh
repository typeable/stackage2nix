#!/usr/bin/env sh

stack2nix --nix-stackage-config=nix/configuration-packages.nix --nix-stackage-packages=./nix/packages.nix

nix-build -A stack2nix --dry-run
