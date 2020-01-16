{ mkDerivation, base, stdenv, vector }:
mkDerivation {
  pname = "example";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base vector ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
