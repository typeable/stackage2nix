{ stdenv, cacert, git, curl, lndir, runCommand
, _cacheVersion ? "0" }:

rec {
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
      git clone --bare --branch hackage https://github.com/commercialhaskell/all-cabal-hashes.git $out
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
  stackage-nightly = stdenv.mkDerivation {
    name = "stackage-nightly";
    version = _cacheVersion;
    phases = [ "installPhase" ];
    buildInputs = [ git ];
    installPhase = ''
      mkdir -p $out
      git clone --depth 1 https://github.com/fpco/stackage-nightly.git $out
    '';
    SSL_CERT_FILE="${cacert}/etc/ssl/certs/ca-bundle.crt";
  };
  stackage-all = runCommand "stackage-all"
    {nativeBuildInputs = [ stackage-nightly stackage-lts lndir ];}
    ''
    mkdir $out
    for i in ${stackage-nightly} ${stackage-lts}; do
      lndir -silent $i $out
    done
    '';
}
