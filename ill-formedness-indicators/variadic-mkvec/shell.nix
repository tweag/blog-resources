{ pkgs ? import (builtins.fetchTarball "https://nixos.org/channels/nixos-22.11/nixexprs.tar.xz") {}
}:

with pkgs;

mkShell rec {
  name = "variadic-mkvec-env";

  ghc = haskell.compiler.ghc8107;

  buildInputs = [ bash cabal-install ghc ];
}
