{ mkDerivation, attoparsec, base, bytestring, cherry-core
, containers, directory, filelock, filepath, network
, optparse-applicative, postgresql-simple, process, resourcet
, stdenv, tasty, tasty-hunit, text, typed-process, unix
}:
mkDerivation {
  pname = "postgresql-fixture";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring cherry-core containers directory
    filelock filepath network postgresql-simple resourcet text
    typed-process unix
  ];
  executableHaskellDepends = [
    base cherry-core directory filepath optparse-applicative process
    unix
  ];
  testHaskellDepends = [
    attoparsec base bytestring cherry-core containers directory
    filelock filepath network postgresql-simple resourcet tasty
    tasty-hunit text typed-process unix
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
