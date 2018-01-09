# stackage2nix

[![Build Status](https://travis-ci.org/typeable/stackage2nix.svg?branch=master)](https://travis-ci.org/typeable/stackage2nix)

`stackage2nix` converts a Stack file into a Nix Haskell packages set.

# Create Nix build from stack.yaml

## Generate targets from stack.yaml only

```
stackage2nix .
```

Command creates file `default.nix` which overrides `haskell.packages.stackage`
packages set. Currently Nixpkgs maintains only Hackage packages, but you can
use [4e6/nixpkgs-stackage](https://github.com/4e6/nixpkgs-stackage) overlay that
adds LTS Stackage packages.

Build package with overlay installed:

```
nix-build -A stackage2nix
```

## Generate Stackage packages for the build

If you don't want to use Stackage overlay, stackage2nix can generate required
packages with `--with-stackage-closure` flag.

``` bash
stackage2nix \
  --all-cabal-hashes /path/to/commercialhaskell/all-cabal-hashes \
  --lts-haskell /path/to/fpco/lts-haskell \
  --with-stackage-closure
  ./stack.yaml
```

To generate Stackage packages, you should supply additional
`--all-cabal-hashes` and `--lts-haskell` flags that points to
[commercialhaskell/all-cabal-hashes](https://github.com/commercialhaskell)
checked out to `hackage` branch and
[fpco/lts-haskell](https://github.com/fpco/lts-haskell) respectively.

You can use stackage2nix wrapper from `nix` directory that adds required flags:

```
nix-env -i -f ./nix/stackage2nix
stackage2nix --with-stackage ./stack.yaml
```

This command will produce `packages.nix` and `configuration-packages.nix`
Stackage packages and its override in `default.nix`

## Generate full Stackage

`--with-stackage` parameter generates full Stackage lts in addition to the
targets from `stack.yaml`

```
nix-env -i -f ./nix/stackage2nix
stackage2nix --with-stackage ./stack.yaml
```

## Generate Stackage from LTS resolver

You can also generate only Stackage packages from the resolver:

```
nix-env -i -f ./nix/stackage2nix
stackage2nix --resolver lts-10.0
```

This command will produce Stackage packages `packages.nix`, packages config
`configuration-packages.nix` and a Nix Haskell packages set `default.nix`.

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
with haskell.lib;
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

Integration tests that build stackage2nix form different yaml configs:

```
STACKAGE_REPO=<path/to/stackage/repo> \
ALL_CABAL_HASHES=<path/to/all-cabal-hashes/repo> \
STACK_FILE=stack-ghc-7103.yaml \
./ci-stackage2nix
```
