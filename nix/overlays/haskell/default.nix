self: super: {
  customHaskell = self.haskellPackages.extend
    (haskellSelf: haskellSuper:
      let
        packageFromGitHub = owner: name: rev:
          let
            src = builtins.fetchGit {
              url = "git@github.com:${owner}/${name}";
              rev = rev;
            };
          in haskellSelf.callCabal2nix name src {};
      in {
        cherry-core = packageFromGitHub "cherry-haskell" "cherry-core"
          "bfe276a41bfa70e475863ad6d6c68fc9223fda32";
      });
  customGhc = self.customHaskell.ghcWithPackages
    (haskellPackages:
      with haskellPackages; [
        cherry-core
      ]);
}
