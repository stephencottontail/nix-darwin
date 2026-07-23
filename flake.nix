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
                vimr = pkgs.callPackage ./vim-refined/package.nix { };
              })
              (final: prev: {
                vimPlugins = prev.vimPlugins // {
                  conjure = prev.vimPlugins.conjure.overrideAttrs (old: {
                    pname = "conjure";
                    version = "4.60.0";
                    doCheck = false;

                    src = prev.fetchFromGitHub {
                      owner = "Olical";
                      repo = "conjure";
                      tag = "v4.60.0";
                      hash = "sha256-wz+nHMR6gYXGDxSAZExd7CItONY2MERYzDapNpKFLmc=";
                    };
                  });
                };
              })
            ];
            config.allowUnfree = true;
            config.input-fonts.acceptLicense = true;
          };

          # TouchID for sudo
          security.pam.services.sudo_local.touchIdAuth = true;

          # List packages installed in system profile. To search by name, run:
          # $ nix-env -qaP | grep wget
          environment.systemPackages = [
            pkgs.nixfmt
            pkgs.zoom-us
            pkgs.sciteco
            pkgs.ibiblio-teco
            pkgs.devenv
            pkgs.groff
            pkgs.groff.perl
            pkgs.typescript-language-server
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

          # ZSH
          programs.zsh = {
            enable = true;
            promptInit = ''
              export PROMPT="%m> "
            '';
          };

          # Extra environment variables
          environment.variables = {
            PATH = "$PATH:/Users/stephen/bin";
          };

          # Extra fonts
          fonts.packages = [
            pkgs.cm_unicode
            pkgs.vt323
            pkgs.nerd-fonts.bigblue-terminal
            pkgs.nerd-fonts.hasklug
            pkgs.nerd-fonts._3270
            pkgs.input-fonts
          ];

          # The platform the configuration will be used on.
          nixpkgs.hostPlatform = "aarch64-darwin";
        };
      homeConfiguration =
        { lib, pkgs, ... }:
        let
          grammars = [
            pkgs.tree-sitter-grammars.tree-sitter-commonlisp
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
            pkgs.vimr
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
            neovim = {
              enable = true;
              extraConfig = ''
                source $HOME/.vim/vimrc
  
                set runtimepath^=${pkgs.vimPlugins.nvim-treesitter}/runtime
                " set packpath^=$HOME/.vim/pack
  
                lua << EOF
                  vim.lsp.enable({ "ts_ls" })
  
                  vim.api.nvim_create_autocmd('DiagnosticChanged', {
                    callback = function()
                      vim.diagnostic.setloclist({ open = false })
                    end
                  })
                EOF
              '';
              plugins = with pkgs.vimPlugins; [
                nvim-lspconfig
                nvim-treesitter
                nvim-treesitter-parsers.commonlisp
                nvim-treesitter-parsers.typescript
                nvim-treesitter-parsers.tsx
                conjure
              ];
              withRuby = false;
              withPython3 = false;
            };
            direnv = {
              enable = true;
              nix-direnv.enable = true;
            };
            emacs = {
              enable = true;
              package = pkgs.emacs-macport;
              extraPackages = epkgs: [
                epkgs.envrc
                epkgs.org
                epkgs.meow
                epkgs.meow-tree-sitter
                epkgs.slime
                epkgs.paredit
                (epkgs.callPackage ./symex/package.nix { })
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
                init.defaultBranch = "main";
                color.ui = false;
              };
            };
          };

          # Emacs
          home.file.".config/emacs/init.el" = {
            source = dotfiles/init.el;
          };

          # ZSH
          home.file.".zshrc".text = ''
            eval "$(direnv hook zsh)"
          '';

          home.file = {
            # TODO: at some point I should write another map that pulls out
            # just the query files I need and symlinks them into
            # `$XDG_CONFIG_HOME`
            ".vim/vimrc" = {
              source = dotfiles/vimrc;
            };
            ".config/nvim/after/ftplugin/javascript.vim" = {
              text = ''
                setlocal shiftwidth=4
                setlocal tabstop=4
                setlocal noexpandtab
              '';
            };
            ".config/nvim/after/ftplugin/typescript.vim" = {
              text = ''
                source $HOME/.config/nvim/after/ftplugin/javascript.vim
              '';
            };
            ".config/nvim/after/ftplugin/lisp.vim" = {
              text = ''
                packadd conjure
              '';
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
