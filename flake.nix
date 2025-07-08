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
              nativeBuildInputs =
                [
                  pkgs.coreutils
                  pkgs.findutils
                  pkgs.gnumake
                  pkgs.nodejs_22
                ]
                # The version of PureScript we use doesn't have pre-built binaries for ARM architectures.
                # The `npm` package will fall back to building from source,
                # but we need somre more dependencies in order to do that.
                # We should be able to remove this once we update to at least version [0.15.9](https://github.com/purescript/purescript/releases/tag/v0.15.9) of PureScript,
                # as this was the first version to start producing pre-built ARM binaries: https://github.com/purescript/purescript/pull/4455.
                ++ pkgs.lib.lists.optionals pkgs.stdenv.hostPlatform.isAarch [
                  pkgs.llvmPackages_12.libcxxClang
                  pkgs.llvmPackages_12.libllvm
                  pkgs.stack
                ];
            };
          };
        };

      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
    };
}
