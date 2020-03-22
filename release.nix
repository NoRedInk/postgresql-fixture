let
  pkgs = import ./nix/pkgs.nix;
in
pkgs.customHaskell.callPackage ./default.nix { }
