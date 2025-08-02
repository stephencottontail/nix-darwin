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
      home-manager
    }:
    let
      configuration =
        { pkgs, ... }:
        {
          # nix-darwin manages nix
          # This is necesary to use the linux-builder so we can build VMs
          nix = {
            enable =false;
            settings.experimental-features = "nix-command flakes";
            settings.trusted-users = [ "@admin" ];
          };

          # Special config for `nixpkgs`
          nixpkgs = {
            overlays = [
              (import ./overlay.nix)
              (final: prev: {
                fuse-t = pkgs.callPackage ./fuse-t/package.nix {};
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
            pkgs.fuse-t
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
          };

          # Extra fonts
          fonts.packages = [
            pkgs.cm_unicode
          ];

          # Copy `fuse-t` files into place
          system.activationScripts.extraActivation = {
            enable = true;
            text = ''
              cp -r ${pkgs.fuse-t}/Library/Application\ Support/fuse-t/ /Library/Application\ Support/
              cp -r ${pkgs.fuse-t}/Library/Frameworks/fuse_t.framework /Library/Frameworks/
              mkdir -p /usr/local/bin
              mkdir -p /usr/local/lib
              mkdir -p /usr/local/lib/pkgconfig
              ln -sf ${pkgs.fuse-t}/Library/Application\ Support/fuse-t/bin/go-nfsv4-1.0.47 /usr/local/bin/go-nfsv4
              cp ${pkgs.fuse-t}/Library/Application\ Support/fuse-t/lib/libfuse-t-1.0.47.dylib /usr/local/lib/
              cp ${pkgs.fuse-t}/Library/Application\ Support/fuse-t/lib/libfuse-t-1.0.47.a /usr/local/lib/
              ln -sf /usr/local/lib/libfuse-t-1.0.47.dylib /usr/local/lib/libfuse-t.dylib
              ln -sf /usr/local/lib/libfuse-t-1.0.47.a /usr/local/lib/libfuse-t.a
              cp ${pkgs.fuse-t}/Library/Application\ Support/fuse-t/pkgconfig/fuse-t.pc /usr/local/lib/pkgconfig/

              if ! grep -q "^127\.0\.0\.1\s*fuse-t" /etc/hosts; then
                echo "127.0.0.1 fuse-t" >> /etc/hosts
              fi
            '';
          };

          # The platform the configuration will be used on.
          nixpkgs.hostPlatform = "aarch64-darwin";
        };
      homeConfiguration =
        { pkgs, ... }:
        {
          # This is apparently required for internal compatibility
          # Don't mess with it?
          home.stateVersion = "23.05";

          programs.home-manager.enable = true;

          # ZSH
          home.file.".zshrc".text = ''
            # Empty file to suppress new user menu
          '';

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
