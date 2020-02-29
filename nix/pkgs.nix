import <nixpkgs> {
  overlays = [
    (import ./overlays/haskell)
    (import ./overlays/ormolu)
  ];
}
