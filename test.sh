#!/usr/bin/env bash

VERSION=${VERSION:-8.15}

stack2nix \
  --stackage-build-plan "$HOME/projects/fpco/lts-haskell/lts-$VERSION.yaml" \
  --all-cabal-hashes "$HOME/projects/commercialhaskell/all-cabal-hashes" \
  --nix-stackage-config=nix/configuration-packages.nix \
  --nix-stackage-packages=./nix/packages.nix

nix-build -A stack2nix --dry-run
