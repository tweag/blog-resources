let
  jupyterLib = builtins.fetchGit {
    url = https://github.com/tweag/jupyterWith;
    rev = "cd0743170d9da4a9e59fad8daf17dd4769bd2158"; 
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
      extraIHaskellFlags = "--codemirror Haskell";
    };

  jupyterlabWithKernels =
    jupyter.jupyterlabWith {
      kernels = [ ihaskellWithPackages ];
    };
in
  jupyterlabWithKernels.env
