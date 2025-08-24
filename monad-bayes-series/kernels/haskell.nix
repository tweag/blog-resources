{
  availableKernels,
  name,
  extraArgs,
}:
availableKernels.haskell {
  inherit name;
  inherit (extraArgs) pkgs;
  displayName = "Monad-Bayes Haskell Kernel";
  extraHaskellPackages = p: [
      p.monad-bayes
      p.hmatrix
      p.hvega
      p.statistics 
      p.vector
      p.ihaskell-hvega
      p.formatting
      p.foldl
      p.histogram-fill
   ];
}