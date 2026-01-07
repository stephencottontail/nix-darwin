{
  description = "Example nix-darwin system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin.url = "github:dwt/nix-darwin/application-linking-done-right";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    inputs@{
      self,
      nix-darwin,
      nixpkgs,
      home-manager,
    }:
    let
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
            overlays = [
              (import ./overlay.nix)
              (final: prev: {
                jasspa-uemacs = pkgs.callPackage ./jasspa-uemacs/package.nix {};
              })
              (final: prev: {
                sciteco = pkgs.callPackage ./sciteco/package.nix {};
              })
              (final: prev: {
                ibiblio-teco = pkgs.callPackage ./ibiblio-teco/package.nix {};
              })
              (final: prev: {
                vimr = pkgs.callPackage ./vim-refined/package.nix {};
              })
            ];
            config.allowUnfree = true;
          };

          # TouchID for sudo
          security.pam.services.sudo_local.touchIdAuth = true;

          # List packages installed in system profile. To search by name, run:
          # $ nix-env -qaP | grep wget
          environment.systemPackages = [
            pkgs.plan9port
            pkgs.nixfmt-rfc-style
            pkgs.zoom-us
            pkgs.jasspa-uemacs
            pkgs.emacs
            pkgs.sciteco
            pkgs.ibiblio-teco
            pkgs.devenv
            pkgs.vimr
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
            PLAN9 = "${pkgs.plan9port}/plan9";
            PATH = "$PATH:/Users/stephen/bin";
          };

          # Extra fonts
          fonts.packages = [
            pkgs.cm_unicode
            pkgs.vt323
            pkgs.terminus_font_ttf
          ];

          # The platform the configuration will be used on.
          nixpkgs.hostPlatform = "aarch64-darwin";
        };
      homeConfiguration =
        { pkgs, ... }:
        {
          # This is apparently required for internal compatibility
          # Don't mess with it?
          home.stateVersion = "23.05";

          programs = {
            home-manager.enable = true;
            git = {
              enable = true;
              userName = "Stephen Dickinson";
              userEmail = "stephencottontail@me.com";
              extraConfig = {
                core.editor = "vim";
                init.defaultBranch = "main";
                color.ui = false;
              };
            };
          };

          # ZSH
          home.file.".zshrc".text = ''
            # Empty file to suppress new user menu
          '';

          # Scripts
          home.file."bin/a" = {
            executable = true;
            text = ''
              #!${pkgs.plan9port}/plan9/bin/rc

              font="${pkgs.plan9port}/plan9/font/pelm/ascii.12.font"
              BROWSER="safari"
              TERM="dumb"
              PAGER="nobs"

              ${pkgs.plan9port}/plan9/bin/9 acme -a -f ${pkgs.plan9port}/plan9/font/pelm/ascii.12.font $* &
            '';
          };

          home.file."bin/pretty" = {
            executable = true;
            text = ''
              #!${pkgs.plan9port}/plan9/bin/rc

              base=`{dirname $%}

              while (! test -d $base/node_modules) {
                test $base = / && echo "Not a Node project" && exit 1

                base=`{dirname $base}
              }

              $base/node_modules/.bin/prettier --write $%
              echo get | 9p write acme/$winid/ctl
              echo put | 9p write acme/$winid/ctl
            '';
          };

          home.file."bin/eslint" = {
            executable = true;
            text = ''
              #!${pkgs.plan9port}/plan9/bin/rc

              base=`{dirname $%}

              while (! test -d $base/node_modules) {
                test $base = / && echo "Not a Node project" && exit 1

                base=`{dirname $base}
              }

              cat $% | $base/node_modules/.bin/eslint --stdin --stdin-filename $%
            '';
          };

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
