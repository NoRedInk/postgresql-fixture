{ mkDerivation, attoparsec, base, containers, directory, filepath
, network, optparse-applicative, postgresql-simple, process
, resourcet, stdenv, text, typed-process, unix
}:
mkDerivation {
  pname = "postgresql-fixture";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base containers directory filepath network
    postgresql-simple resourcet text typed-process unix
  ];
  executableHaskellDepends = [
    base directory filepath optparse-applicative process unix
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
