{
  description = "Example nix-darwin system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin.url = "github:nix-darwin/nix-darwin/master";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    emacs-plus-src = {
      url = "github:d12frosted/homebrew-emacs-plus";
      flake = false;
    };
  };

  outputs =
    inputs@{
      self,
      nix-darwin,
      nixpkgs,
      home-manager,
      emacs-plus-src,
    }:
    let
      iconSrc = "${emacs-plus-src}/community/icons/liquid-glass";
      starScript = ''
        setlocal shiftwidth=4
        setlocal tabstop=4
        setlocal noexpandtab

        nnoremap K :ALEHover<cr>
        nnoremap gd :ALEGoToDefinition<cr>
        nnoremap gr :ALEFindReferences<cr>
        nnoremap g] :ALENext<cr>
        nnoremap g[ :ALEPrevious<cr>
      '';
      configuration =
        { pkgs, ... }:
        {
          # Let Determinate manage Nix
          nix = {
            enable = false;
            settings.experimental-features = "nix-command flakes";
          };

          # Special config for `nixpkgs`
          nixpkgs = {
            overlays = import ./overlays ++ [
              (final: prev: {
                emacs-macport = prev.emacs-macport.overrideAttrs (old: {
                  postInstall = (old.postInstall or "") + ''
                    RES_DIR="$out/Applications/Emacs.app/Contents/Resources";
                    PLIST="$out/Applications/Emacs.app/Contents/Info.plist";

                    cp "${iconSrc}/Assets.car" "$RES_DIR/Assets.car";
                    cp "${iconSrc}/Icon.icns" "$RES_DIR/Emacs.icns";
                    /usr/libexec/PlistBuddy -c "Add :CFBundleIconName string Emacs" "$PLIST" || /usr/libexec/PlistBuddy -c "Set :CFBundleIconName Emacs" "$PLIST"
                  '';
                  patches = (old.patches) ++ [
                    (prev.fetchpatch {
                      url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/fix-window-role.patch";
                      sha256 = "sha256-+z/KfsBm1lvZTZNiMbxzXQGRTjkCFO4QPlEK35upjsE=";
                    })
                    (prev.fetchpatch {
                      url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-30/system-appearance.patch";
                      sha256 = "sha256-3QLq91AQ6E921/W9nfDjdOUWR8YVsqBAT/W9c1woqAw=";
                    })
                  ];
                });
              })
              (final: prev: {
                sciteco = pkgs.callPackage ./sciteco/package.nix { };
              })
              (final: prev: {
                ibiblio-teco = pkgs.callPackage ./ibiblio-teco/package.nix { };
              })
              (final: prev: {
                vlime = pkgs.callPackage ./vlime/package.nix { };
              })
            ];
            config.allowUnfree = true;
          };

          # TouchID for sudo
          security.pam.services.sudo_local.touchIdAuth = true;

          # List packages installed in system profile. To search by name, run:
          # $ nix-env -qaP | grep wget
          environment.systemPackages = [
            pkgs.nixfmt
            pkgs.direnv
            pkgs.sciteco
            pkgs.ibiblio-teco
            pkgs.groff
            pkgs.groff.perl
            pkgs.nodejs_24
            pkgs.sbcl
            pkgs.sbclPackages.swank
            pkgs.roswell
            pkgs.python3
          ];

          # Set hostname
          networking.hostName = "blueberry";

          # Set Git commit hash for darwin-version.
          system.configurationRevision = self.rev or self.dirtyRev or null;

          # Used for backwards compatibility, please read the changelog before changing.
          # $ darwin-rebuild changelog
          system.stateVersion = 6;

          # Let's see how far we can go without home-manager
          users.users.stephen = {
            name = "stephen";
            home = "/Users/stephen";
          };

          system.primaryUser = "stephen";

          # The Nix package for `mlx-lm` doesn't seem to work; the server starts and
          # accepts connections but neither `curl` nor `gptel` actually get any response
          # back. Instead, I wrote a script to manually install `mlx-lm` through `pip`.
          launchd.user.agents.mlx = {
            script = ''
              MLX_DIRECTORY="$HOME/Documents/Projects/mlx"

              if [ ! -d "$MLX_DIRECTORY" ]; then
                echo "Creating MLX directory..."
                mkdir -p $MLX_DIRECTORY
              fi

              if [ ! -d "$MLX_DIRECTORY/.venv" ]; then
                echo "Creating virtual environment..."
                ${pkgs.python3}/bin/python -m venv "$MLX_DIRECTORY/.venv"
              fi

              echo "Activating virtual environment..."
              source "$MLX_DIRECTORY/.venv/bin/activate"

              echo "Installing MLX..."
              "$MLX_DIRECTORY/.venv/bin/pip" install --upgrade pip
              "$MLX_DIRECTORY/.venv/bin/pip" install mlx-lm

              echo "Starting server..."
              "$MLX_DIRECTORY/.venv/bin/mlx_lm.server" --model mlx-community/Olmo-3-7B-Instruct-4bit
            '';
            serviceConfig = {
              KeepAlive = true;
              RunAtLoad = true;
              StandardOutPath = "/tmp/mlx.out.log";
              StandardErrorPath = "/tmp/mlx.err.log";
            };
          };

          # ZSH
          programs.zsh = {
            enable = true;
            promptInit = ''
              export PROMPT="%m> "
            '';
          };

          # Extra environment variables
          environment.variables = {
            PLAN9 = "$HOME/.local/plan9port";
            PATH = "$PATH:/Users/stephen/bin";
            ROS_LISP = "sbcl-bin/system";
            ROSWELL_HOME = "$HOME/.config/roswell";
          };

          # Extra fonts
          fonts.packages = [
            pkgs.cm_unicode
            pkgs.dejavu_fonts
            pkgs.go-font
            pkgs.liberation_ttf_v2
            pkgs.nerd-fonts._3270
            pkgs.vt323
          ];

          # The platform the configuration will be used on.
          nixpkgs.hostPlatform = "aarch64-darwin";
        };
      homeConfiguration =
        { lib, pkgs, ... }:
        let
          grammars = [
            pkgs.tree-sitter-grammars.tree-sitter-typescript
            pkgs.tree-sitter-grammars.tree-sitter-tsx
          ];
        in
        {
          # This is apparently required for internal compatibility
          # Don't mess with it?
          home.stateVersion = "23.05";

          home.packages = [
            pkgs.macvim
          ];

          targets.darwin = {
            copyApps = {
              enable = true;
              directory = "Applications";
            };
            linkApps = {
              enable = false;
            };
          };

          programs = {
            home-manager.enable = true;
            direnv = {
              enable = true;
              nix-direnv.enable = true;
            };
            emacs = {
              enable = true;
              package = pkgs.emacs-macport;
              extraPackages = epkgs: [
                (epkgs.ellama.overrideAttrs (old: rec {
                  version = "1.31.0";

                  src = pkgs.fetchFromGitHub {
                    owner = "s-kostyaev";
                    repo = "ellama";
                    rev = "${version}";
                    hash = "sha256-cf1uqlwTdO+koMaHFU+o0jo58mQ5Nevm4sJaSW9L4+A=";
                  };
                }))
                (epkgs.gptel.overrideAttrs (old: rec {
                  version = "0.9.9.5";

                  src = pkgs.fetchFromGitHub {
                    owner = "karthink";
                    repo = "gptel";
                    rev = "f5ad4eb812920fba3fcfbe32042ef89f979a7e17";
                    hash = "sha256-qDPDFK/mODg/nBVCbHEaICIBtjaFdRsOSQXVXK3vx60=";
                  };
                }))
                (epkgs.gptel-agent.overrideAttrs (old: rec {
                  src = pkgs.fetchFromGitHub {
                    owner = "karthink";
                    repo = "gptel-agent";
                    rev = "e833bcaf617baf8c8075eac098231c4457386814";
                    hash = "sha256-gX3n3T/jmmvA25s8qMpOyfQzuSKyY3OgkEkitMXfspg=";
                  };
                }))
                epkgs.envrc
                epkgs.magit
                epkgs.meow
                epkgs.meow-tree-sitter
                epkgs.org
                epkgs.slime
                epkgs.paredit
              ];
            };
            git = {
              enable = true;
              settings = {
                user = {
                  email = "stephencottontail@me.com";
                  name = "Stephen Dickinson";
                };
                core.editor = "vim";
                core.pager = "cat";
                init.defaultBranch = "main";
                color.ui = false;
              };
            };
          };

          # Emacs
          home.file.".config/emacs/tree-sitter" = {
            recursive = true;
            source = pkgs.runCommand "emacs-treesitter-grammars" { } ''
              mkdir -p $out

              ${pkgs.lib.concatStringsSep "\n" (
                builtins.map (grammar: ''
                  CLEAN_NAME=$(echo "${grammar.pname}" | sed 's/^tree-sitter-//')

                  if [ -f "${grammar}/parser" ]; then
                    ln -s "${grammar}/parser" "$out/libtree-sitter-$CLEAN_NAME.dylib.0"
                  fi
                '') (grammars)
              )}
            '';
          };

          home.file.".config/emacs/init.el" = {
            source = dotfiles/init.el;
          };

          # ZSH
          home.file.".zshrc".text = ''
            # It seems that we can set `PLAN9` in `environment.variables`
            # but we then can't also use it to set `PATH` in `env` at
            # at the same time
            export PATH=$PATH:$PLAN9/bin

            eval "$(direnv hook zsh)"
          '';

          home.file = {
            ".vim/vimrc" = {
              source = dotfiles/vimrc;
            };
            ".vim/after/ftplugin/javascript.vim" = {
              text = starScript;
            };
            ".vim/after/ftplugin/typescript.vim" = {
              text = starScript;
            };
            # Vim packages
            #
            # We do this because we can't use `pkgs.macvim`
            # as the `packageConfigurable` for
            # `home.programs.vim.*`, but I still wanted Nix
            # to manage Vim packages
            #
            # TODO: Check if `packDir` supports the newer
            #       syntax
            # TODO: ^ what
            ".vim/pack" = {
              source = pkgs.vimUtils.packDir {
                "hm-vim-packages" = {
                  start = [
                    pkgs.vimPlugins.ale
                    pkgs.vlime
                  ];
                };
              };
            };

            ".vim/colors" = {
              source = dotfiles/colors;
            };
          };

          # Scripts
          home.file."bin/update" = {
            executable = true;
            text = ''
              #!/usr/bin/env sh

              sudo darwin-rebuild switch --flake /Users/stephen/Documents/Projects/nix-darwin/
            '';
          };

        };
    in
    {
      # Build darwin flake using:
      # $ darwin-rebuild build --flake .
      #
      # Since the hostname and this configuration name match, we don't
      # need to explicitly name the configuration in the command
      darwinConfigurations."blueberry" = nix-darwin.lib.darwinSystem {
        modules = [
          configuration
          home-manager.darwinModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.verbose = true;
            home-manager.users.stephen = homeConfiguration;
          }
        ];
      };
    };
}
