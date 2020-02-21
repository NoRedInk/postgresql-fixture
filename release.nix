let
  pkgs = import ./nix/pkgs.nix;
in
pkgs.haskellPackages.callPackage ./default.nix { }
