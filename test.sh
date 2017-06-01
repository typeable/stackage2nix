#!/usr/bin/env sh

exec stack2nix --nix-stackage-config=nix/configuration-packages.nix --nix-stackage-packages=./nix/packages.nix
