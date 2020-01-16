let
  pkgs = import <nixpkgs> {};

  haskellPackagesWithProfiling = flag: pkgs.haskellPackages.override {
    overrides = self: super: {
      mkDerivation = args: super.mkDerivation (args // {
        enableExecutableProfiling = true;
        enableLibraryProfiling = true;
        configureFlags = if flag then [ "--ghc-option=-fno-prof-auto" ] else [];
      });
    };
  };
  normal = pkgs.haskellPackages.callPackage ./. {};
  profiling = (haskellPackagesWithProfiling true).callPackage ./. {};
  profilingAuto = (haskellPackagesWithProfiling false).callPackage ./. {};

in

{ inherit normal profiling profilingAuto }
