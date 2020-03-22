{ pkgs ? import ./nix/pkgs.nix }:
let
  inherit (pkgs) haskellPackages;
  project = import ./release.nix;
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = project.env.nativeBuildInputs ++ [
    haskellPackages.cabal-install
    pkgs.postgresql
    pkgs.ormolu
  ];
}
