{ modulesPath, pkgs, ... }:
{
  imports = [
    "${modulesPath}/virtualisation/google-compute-image.nix"
  ];
  virtualisation.googleComputeImage.diskSize = 3000;
  virtualisation.docker.enable = true;

  services = {
    buildkite-agents.agent = {
      enable = true;
      extraConfig = ''
      tags-from-gcp=true
      '';
      tags = {
        os = "nixos";
        nix = "true";
      };
      tokenPath = "/run/keys/buildkite-agent-token";
      runtimePackages = with pkgs; [
        bash
        curl
        gcc
        gnutar
        gzip
        ncurses
        nix
        python3
        xz
        # extend as necessary
      ];
    };
    nix-store-gcs-proxy = {
      nix-cache-bucket-name = {
        address = "localhost:3000";
      };
    };
  };

  nix = {
    binaryCaches = [
      "https://cache.nixos.org/"
      "https://storage.googleapis.com/nix-cache-bucket-name"
    ];
    binaryCachePublicKeys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "<insert your public signing key here>"
    ];
    extraOptions = ''
      post-build-hook = /etc/nix/upload-to-cache.sh
    '';
  };

  security.sudo.enable = true;
  services.openssh.passwordAuthentication = false;
  security.sudo.wheelNeedsPassword = false;
}
