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

    git-hooks_nix = {
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };

      owner = "cachix";

      ref = "master";

      repo = "git-hooks.nix";

      type = "github";
    };

    nixpkgs = {
      owner = "NixOS";

      ref = "nixos-unstable";

      repo = "nixpkgs";

      type = "github";
    };

    treefmt-nix = {
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };

      owner = "numtide";

      ref = "main";

      repo = "treefmt-nix";

      type = "github";
    };
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        # The `git-hooks_nix` module will play nicely with the `treefmt-nix` module.
        # It will take the configuration we supply,
        # and use it appropriately instead of competing with it.
        inputs.git-hooks_nix.flakeModule
        # The `treefmt-nix` module will set the `formatter` to `treefmt`.
        # This lets `nix fmt` use our configuration for `treefmt`.
        inputs.treefmt-nix.flakeModule
      ];

      perSystem =
        { config, pkgs, ... }:
        {
          devShells = {
            default = pkgs.mkShellNoCC {
              inputsFrom = [
                # Include the shell from `git-hooks_nix`.
                # This makes all `pre-commit` binaries (include `pre-commit`) available in the shell.
                config.pre-commit.devShell
                # Include the shell from `treefmt`.
                # This makes all `treefmt` binaries (including `treefmt`) available in the shell.
                config.treefmt.build.devShell
              ];

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

          pre-commit = {
            settings = {
              default_stages = [
                "pre-commit"
                "pre-push"
              ];

              hooks = {
                # Check Nix code for anything that is unused.
                deadnix = {
                  enable = true;
                };

                # Check that we're not accidentally committing AWS credentials.
                detect-aws-credentials = {
                  enable = true;
                };

                # Check that we're not accidentally committing private keys
                detect-private-keys = {
                  enable = true;
                };

                # Git submodules are nothing but a can of worms that fails in some non-obvious way each time they're used.
                forbid-new-submodules = {
                  enable = true;
                };

                # Check markdown files for consistency.
                markdownlint = {
                  enable = true;

                  settings = {
                    configuration = {
                      MD013 = {
                        # We'd like to use something like `wrap:inner-sentence`:
                        # https://cirosantilli.com/markdown-style-guide/#option-wrap-inner-sentence,
                        # or something related to SemBr: https://sembr.org/.
                        # But that's stymied in an issue: https://github.com/DavidAnson/markdownlint/issues/298.
                        #
                        # We set the line length to something large enough to not get hit by it regularly.
                        line_length = 1000;
                      };
                    };
                  };
                };

                # While `nil` is a language server,
                # it also has come static analysis we want to check.
                nil = {
                  enable = true;
                };

                # Check for any common mistakes in Shell code we write.
                shellcheck = {
                  enable = true;
                };

                # Check that we're not making any simple typos in prose.
                # This checks not just plain text files (like Markdown),
                # but also comments in source code like this comment you're reading right now.
                typos = {
                  enable = true;
                };
              };
            };
          };

          treefmt = {
            programs = {
              # Format JSON code consistently.
              jsonfmt = {
                enable = true;
              };

              # Format Markdown code consistently.
              mdformat = {
                enable = true;
              };

              # Format Nix code consistently.
              nixfmt = {
                enable = true;
              };

              # Format Shell code consistently.
              shfmt = {
                enable = true;
              };
            };

            settings.global.excludes = [
              ".direnv/*"
              ".jj/*"
              "bower_components/*"
              "node_modules/*"
              "output/*"
            ];
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
