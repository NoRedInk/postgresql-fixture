{ mkDerivation, base, directory, filepath, process, stdenv, unix }:
mkDerivation {
  pname = "postgresql-fixture";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base directory filepath process unix ];
  executableHaskellDepends = [
    base directory filepath process unix
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
