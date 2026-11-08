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
                emacs = prev.emacs.overrideAttrs (old: {
                  postInstall = (old.postInstall or "") + ''
                    RES_DIR="$out/Applications/Emacs.app/Contents/Resources";
                    PLIST="$out/Applications/Emacs.app/Contents/Info.plist";

                    cp "${iconSrc}/Assets.car" "$RES_DIR/Assets.car";
                    cp "${iconSrc}/Icon.icns" "$RES_DIR/Emacs.icns";
                    /usr/libexec/PlistBuddy -c "Add :CFBundleIconName string Emacs" "$PLIST" || /usr/libexec/PlistBuddy -c "Set :CFBundleIconName Emacs" "$PLIST"
                  '';
                  patches = (old.patches) ++ [
                    (prev.fetchpatch {
                      url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/community/patches/aggressive-read-buffering/emacs-31.patch";
                      sha256 = "sha256-4w8/310XDuLROZfiwxfnqmBWZtinIT3EomcKLh8d58A=";
                    })
                    (prev.fetchpatch {
                      url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-31/fix-ns-x-colors.patch";
                      sha256 = "sha256-oe3DFgEXwp0cZJl+ufWqTonaeWSliikTRsVDNbcy4Yw=";
                    })
                    (prev.fetchpatch {
                      url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-31/system-appearance.patch";
                      sha256 = "sha256-4+2U+4+2tpuaThNJfZOjy1JPnneGcsoge9r+WpgNDko=";
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
            pkgs.ollama
            pkgs.sbcl
            pkgs.sbclPackages.swank
            pkgs.roswell
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

          launchd.user.agents.ollama = {
            script = "${pkgs.ollama}/bin/ollama serve";
            serviceConfig = {
              KeepAlive = true;
              RunAtLoad = true;
              StandardOutPath = "/tmp/ollama.out.log";
              StandardErrorPath = "/tmp/ollama.err.log";
            };
          };

          # ZSH
          programs.zsh = {
            enable = true;
            promptInit = ''
              export PROMPT="%# "
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
            pkgs.atkinson-hyperlegible
            pkgs.atkinson-monolegible
            pkgs.cm_unicode
            pkgs.dejavu_fonts
            pkgs.intel-one-mono
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
              package = pkgs.emacs;
              extraPackages = epkgs: [
                (epkgs.ellama.overrideAttrs (old: rec {
                  src = pkgs.fetchFromGitHub {
                    owner = "s-kostyaev";
                    repo = "ellama";
                    rev = "b98cb4a904b70def4f21050d5d202005ee00b3b4";
                    hash = "sha256-YPh6fXvU0iUqgR35ZyitEE/rTDf5KwKd3s/IlicRiOo=";
                  };
                }))
                (epkgs.gptel.overrideAttrs (old: rec {
                  src = pkgs.fetchFromGitHub {
                    owner = "karthink";
                    repo = "gptel";
                    rev = "ec25a41fb8bebf5ea08341a9d8c70c0ee907ee23";
                    hash = "sha256-YJhjFsrFgUqk4j8Lfq4g5H9sAdqOcfiWHx4HWyUCQ/c=";
                  };
                }))
                (epkgs.gptel-agent.overrideAttrs (old: rec {
                  src = pkgs.fetchFromGitHub {
                    owner = "karthink";
                    repo = "gptel-agent";
                    rev = "326e0abb8c6db0e5c4c421bd9af495932ae94b62";
                    hash = "sha256-2eqY1MebzsDOpf3YEHy6o31TbpLA2iIPZMMVrscLYMk=";
                  };
                }))
                epkgs.envrc
                epkgs.magit
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
