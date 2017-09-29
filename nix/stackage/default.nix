{ nixpkgs ? import <nixpkgs> {}
, lts ? "lts-9.0" }:

with nixpkgs;
let
  lib = callPackage ../lib.nix {};
  stackage2nix = import ../stackage2nix { inherit nixpkgs; };
in stdenv.mkDerivation {
  name = "stackage";
  phases = [ "installPhase" ];
  buildInputs = [ stackage2nix ];

  installPhase = ''
    export HOME=$TMP
    mkdir -p $out
    pushd $out
    stackage2nix --resolver ${lts}
    popd
  '';

  LANG="en_US.UTF-8";
  SSL_CERT_FILE="${cacert}/etc/ssl/certs/ca-bundle.crt";
  NIX_PATH="nixpkgs=${nixpkgs.path}";
}
