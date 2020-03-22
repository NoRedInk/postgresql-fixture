.PHONY:
build: release.nix
	nix-build release.nix --no-out-link

.PHONY:
shell: shell.nix
	nix-shell $^

.PHONY:
repl: shell.nix
	nix-shell --pure $^ --run "cabal repl"

.PHONY:
format:
	git ls-files -z 'src/*.hs' | xargs -0n1 -- ormolu --mode=inplace

shell.nix: release.nix

release.nix: default.nix

default.nix: postgresql-fixture.cabal
	nix-shell --pure -p cabal2nix --run "cabal2nix ." > $@
