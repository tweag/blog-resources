{
  description = "Probabilistic Programming in Haskell blog resourses";
  inputs = {
    nixpkgs.url = "nixpkgs/22.05";
    jupyterWith.url = "github:tweag/jupyterWith";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, jupyterWith, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        inherit (nixpkgs) lib;
        pkgs = import nixpkgs {
          system = system;
          overlays = (nixpkgs.lib.attrValues jupyterWith.overlays) ++ [ (import ./haskell-overlay.nix) ];
        };
        iHaskell = pkgs.kernels.iHaskellWith {
          name = "monad-bayes-series-env";
          packages = p: with p; [
            monad-bayes
            hmatrix
            hvega
            statistics 
            vector
            ihaskell-hvega
            formatting
            foldl
            histogram-fill
          ];
          haskellPackages = pkgs.haskell.packages.ghc865;
          extraIHaskellFlags = "--codemirror Haskell";
        };
        jupyterEnvironment = pkgs.jupyterlabWith {
          kernels = [ iHaskell ];
        };
      in rec {
        apps.jupyterlab = {
          type = "app";
          program = "${jupyterEnvironment}/bin/jupyter-lab";
        };
        defaultApp = apps.jupyterlab;
        devShell = jupyterEnvironment.env;
      }
    );
}
