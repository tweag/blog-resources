let
  jupyterLib = builtins.fetchGit {
    url = https://github.com/tweag/jupyterWith;
    rev = "b0b4e55da09973a57b82200789816f050a970f3e";
  };
  nixpkgsPath = jupyterLib + "/nix";
  haskellOverlay = import ./haskell-overlay.nix;
  pkgs = import nixpkgsPath {overlays = [ haskellOverlay ]; config={allowUnfree=true; allowBroken=true;};};

  jupyter = import jupyterLib {pkgs=pkgs;};

  ihaskellWithPackages = jupyter.kernels.iHaskellWith {
      #extraIHaskellFlags = "--debug";
      haskellPackages = pkgs.haskell.packages.ghc865;
      name = "monad-bayes";
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
    };

  jupyterlabWithKernels =
    jupyter.jupyterlabWith {
      kernels = [ ihaskellWithPackages ];
      directory = jupyter.mkDirectoryWith {
        extensions = [
          "jupyterlab-ihaskell"
        ];
      };
    };
in
  jupyterlabWithKernels.env
