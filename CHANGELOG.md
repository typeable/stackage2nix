# 0.7.1
- Generate stack packages `stack-yaml-packages.nix`

# 0.7.0
- Add compatibility with nixos-18.03
- Drop compatibility with nixos-17-09
- Add partial support for new stack extra-deps syntax.
  https://docs.haskellstack.org/en/stable/yaml_configuration/#packages-and-extra-deps
  Supported same syntax for packages and extra-deps sections

# 0.6.1
- Pass extra `buildHaskellPackages` argument to `makePackageSet` function

# 0.6.0
- Replace call to `<nixpkgs/pkgs/development/haskell-modules>` in generated
  Haskell packages with `makePackageSet` library function

# 0.5.0
- Add `--with-stackage` and `--with-stackage-closure` flag generates stackage
- Add `--extra-deps-revision-latest` flag. Changes generation strategy for the
  direct dependencies of the stack.yaml extra-deps. Default strategy is to
  generate exact revision defined in the Stackage config. With this flag
  enabled, direct dependencies of the stack.yaml extra-deps will be generated
  with latest revision available on Hackage. This may help when Cabal fails to
  resolve dependencies of extra-deps, and dependences are fixed in the latest
  revision
- Upd by default stackage2nix generates override of pre-generated Stackage
  packages set.
- Upd generated Stackage packages with `doCheck` and `runHaddock` enabled by
  default
- Remove `--do-check-stackage` flag (enabled by default)
- Remove `--do-haddock-stackage` flag (enabled by default)
- Remove `--no-packages-closure` flag (use `--with-stackage-closure`)
- Remove `cabal2nix < 2.7.2` support #45

# 0.4.0
- Add `--resolver` option to generate full stackage packages set
- Add Nix wrapper, see `nix/README.md`
- Upd support cabal2nix >2.5
- Fix honor `--hackage-db` flag

# 0.3.0
- Initial Hackage release
- Add CHANGELOG.md
