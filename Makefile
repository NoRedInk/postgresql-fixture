.PHONY:
build: release.nix
	nix-build release.nix

.PHONY:
shell: shell.nix
	nix-shell $^

.PHONY:
repl: shell.nix
	nix-shell --pure $^ --run "cabal repl"

shell.nix: release.nix

release.nix: default.nix

default.nix: postgresql-fixture.cabal
	nix-shell --pure -p cabal2nix --run "cabal2nix ." > $@
