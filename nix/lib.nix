{ stdenv, cacert, git, curl
, _cacheVersion ? "0" }:

{
  hackage-db = stdenv.mkDerivation {
    name = "hackage-db";
    version = _cacheVersion;
    phases = [ "installPhase" ];
    buildInputs = [ curl ];
    installPhase = ''
      mkdir -p $out
      pushd $out
      curl -o 01-index.tar.gz https://hackage.haskell.org/01-index.tar.gz
      gunzip --keep 01-index.tar.gz
      popd
    '';
    SSL_CERT_FILE="${cacert}/etc/ssl/certs/ca-bundle.crt";
  };
  all-cabal-hashes = stdenv.mkDerivation rec {
    name = "all-cabal-hashes";
    version = _cacheVersion;
    phases = [ "installPhase" ];
    buildInputs = [ git ];
    installPhase = ''
      mkdir -p $out
      git clone --branch hackage https://github.com/commercialhaskell/all-cabal-hashes.git $out
    '';
    SSL_CERT_FILE="${cacert}/etc/ssl/certs/ca-bundle.crt";
  };
  stackage-lts = stdenv.mkDerivation {
    name = "stackage-lts";
    version = _cacheVersion;
    phases = [ "installPhase" ];
    buildInputs = [ git ];
    installPhase = ''
      mkdir -p $out
      git clone --depth 1 https://github.com/fpco/lts-haskell.git $out
    '';
    SSL_CERT_FILE="${cacert}/etc/ssl/certs/ca-bundle.crt";
  };
}
