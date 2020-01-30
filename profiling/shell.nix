with (import <nixpkgs> {});
(haskellPackages.callPackage ./. {}).env
