{
  description = "A data type for optional values";

  inputs = {
    flake-parts = {
      inputs = {
        nixpkgs-lib = {
          follows = "nixpkgs";
        };
      };

      owner = "hercules-ci";

      ref = "main";

      repo = "flake-parts";

      type = "github";
    };

    nixpkgs = {
      owner = "NixOS";

      ref = "nixos-unstable";

      repo = "nixpkgs";

      type = "github";
    };
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem =
        { pkgs, ... }:
        {
          devShells = {
            default = pkgs.mkShellNoCC {
              nativeBuildInputs = [
                pkgs.coreutils
                pkgs.findutils
                pkgs.gnumake
                pkgs.nodejs_22
              ];
            };
          };
        };

      systems = [
        "x86_64-darwin"
        "x86_64-linux"
      ];
    };
}
