let
  # use a specific (although arbitrarily chosen) version of the Nix package collection
  default_pkgs = fetchTarball {
    url = "http://github.com/NixOS/nixpkgs/archive/nixpkgs-unstable.tar.gz";
    # the sha256 makes sure that the downloaded archive really is what it was when this
    # file was written
    sha256 = "0x5j9q1vi00c6kavnjlrwl3yy1xs60c34pkygm49dld2sgws7n0a";
  };
# function header: we take one argument "pkgs" with a default defined above
in { pkgs ? import default_pkgs { } }:
with pkgs;
let
  # we create a Python bundle containing Python 3.9 and a few packages
  pythonBundle =
    python39.withPackages (ps: with ps; [ pymc3 matplotlib numpy ipython ]);
# this is what the function returns: the result of a mkShell call with a buildInputs
# argument that specifies all software to be made available in the shell
in mkShell { buildInputs = [ pythonBundle ]; }
