.DEFAULT_GOAL := help

################################################################################
# Stack

stack-normal: ## Execute with Stack
	mkdir -p .stack-bin
	stack clean
	stack install --local-bin-path .stack-bin
	time .stack-bin/example

stack-profiling: ## Execute with Stack and profiling
	mkdir -p .stack-bin
	stack clean
	stack install --local-bin-path .stack-bin --profile --ghc-options="-fno-prof-auto"
	time .stack-bin/example +RTS -p

stack-profiling-auto: ## Execute with Stack and automatic profiling
	mkdir -p .stack-bin
	stack clean
	stack install --local-bin-path .stack-bin --profile --ghc-options="-fprof-auto"
	time .stack-bin/example +RTS -p

################################################################################
# Cabal

cabal-normal: ## Execute with Cabal
	cabal build
	time cabal run example

cabal-profiling: ## Execute with Cabal and profiling
	cabal build --enable-profiling --ghc-options="-fno-prof-auto"
	time cabal exec example -- +RTS -p -RTS

cabal-profiling-auto: ## Execute with Cabal and automatic profiling
	cabal build --enable-profiling --ghc-options="-fprof-auto"
	time cabal exec example -- +RTS -p -RTS

################################################################################
# Nix

nix-normal: ## Execute with Nix
	nix-build release.nix -A normal
	time result/bin/example

nix-profiling: ## Execute with Nix and profiling
	nix-build release.nix -A profiling
	time result/bin/example +RTS -p -RTS

nix-profiling-auto: ## Execute with Nix and automatic profiling
	nix-build release.nix -A profilingAuto
	time result/bin/example +RTS -p -RTS

################################################################################
# Help

help: ## Show this help.
	@echo -e "$$(grep -hE '^\S+:.*##' $(MAKEFILE_LIST) | sed -e 's/:.*##\s*/:/' -e 's/^\(.\+\):\(.*\)/\\x1b[36m\1\\x1b[m:\2/' | column -c2 -t -s :)"
