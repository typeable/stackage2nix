# stackage2nix

[![Build Status](https://travis-ci.org/typeable/stackage2nix.svg?branch=master)](https://travis-ci.org/typeable/stackage2nix)

`stackage2nix` converts a Stack file into a Nix Haskell packages set.
It creates LTS Stackage packages set, and applies appropriate overrides on top of it.

## Install

You can install `stackage2nix` from the Nix expression:

```
nix-env -i -f ./nix/stackage2nix
```

It provides pre-configured wrapper around the raw executable with runtime
`PATH` and all auxiliary flags set up.

## Build project

Generate derivations from `stack.yaml` config using Nix wrapper:

``` bash
stackage2nix ./stack.yaml
```

if you're using the raw executable, you should supply additional flags. See
section 'Flags' below for details.

This command will result in a Haskell packages set, similar to
`pkgs.haskell.packages.<compiler>`, containing only packages that are required
to build targets listed in `stackage.yaml`. To build a package run:

``` bash
nix-build -A my-package
```

## Build stackage

Generate complete Stackage packages set from resolver:

``` bash
stackage2nix --resolver lts-9.0
```

## Flags

```
stackage2nix \
  --lts-haskell "$LTS_HASKELL_REPO" \
  --all-cabal-hashes "$ALL_CABAL_HASHES_REPO" \
  .
```

`stackage2nix` has three required arguments:
- `--lts-haskell` - path to [fpco/lts-haskell](https://github.com/fpco/lts-haskell)
- `--all-cabal-hashes` - path to [commercialhaskell/all-cabal-hashes](https://github.com/commercialhaskell/all-cabal-hashes) checked out to `hackage` branch
- `.` - path to stack.yaml file or directory

Produced Nix derivation split on following files:
- packages.nix - Base Stackage packages set
- configuration-packages.nix - Compiler configuration
- default.nix - Final Haskell packages set with all overrides applied

A particular package from result Haskell packages set can be built with:

```
nix-build -A <package-name>
```

See also the [blog post](https://blog.typeable.io/posts/2017-08-24-stackage2nix.html)
about history and motivation behind the project.

## Runtime dependencies

- `nix-env` is required to be on PATH by the
  [distribution-nixpkgs](https://hackage.haskell.org/package/distribution-nixpkgs)
  dependency
- `nix-prefetch-scripts` is required on PATH if you have git dependencies in
  `stack.yaml`

## Override result derivation

Complex projects may require some extra customization.
Snippet `override.nix` below shows a minimal example of how to apply additional
overrides on top of Haskell packages set produced by `stackage2nix`.

```
with import <nixpkgs> {};
with pkgs.haskell.lib;
let haskellPackages = import ./. {};
in haskellPackages.override {
  overrides = self: super: {
    stackage2nix = disableSharedExecutables super.stackage2nix;
  };
}
```

```
nix-build -A stackage2nix override.nix
```

For more complex overrides and detailed information on how to work with Haskell packages in Nix, see Nixpkgs manual [User’s Guide to the Haskell Infrastructure](http://nixos.org/nixpkgs/manual/#users-guide-to-the-haskell-infrastructure)

## Tests

Integration tests that build stackage2nix form different stack configs:

```
STACKAGE_REPO=<path/to/stackage/repo> \
ALL_CABAL_HASHES=<path/to/all-cabal-hashes/repo> \
STACK_FILE=stack-ghc-7103.yaml \
./ci-stackage2nix
```
