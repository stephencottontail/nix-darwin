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
      pkgs = nixpkgs.legacyPackages.aarch64-darwin;
      fontsrv = pkgs.writeShellScript "fontsrv.sh" ''
        PLAN9=${pkgs.plan9port}/plan9/bin
        PATH=/bin:/usr/bin:/usr/local/bin:${pkgs.plan9port}/plan9/bin

        $PLAN9/fontsrv &
        $PLAN9/9pfuse `$PLAN9/namespace`/font /Users/stephen/mnt/font &
      '';
      configuration =
        { pkgs, ... }:
        {
          # nix-darwin manages nix
          # This is necesary to use the linux-builder so we can build VMs
          nix = {
            enable = true;
            linux-builder.enable = true;
            settings.experimental-features = "nix-command flakes";
            settings.trusted-users = [ "@admin" ];
          };

          # Special config for `nixpkgs`
          nixpkgs = {
            overlays = [ (import ./overlay.nix) ];
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
          ];

          # Set hostname
          networking.hostName = "blueberry";

          # Set Git commit hash for darwin-version.
          system.configurationRevision = self.rev or self.dirtyRev or null;

          # Used for backwards compatibility, please read the changelog before changing.
          # $ darwin-rebuild changelog
          system.stateVersion = 6;

          # For now at least it looks like you need `users.users` for `home-manager`
          # to work and you need `system.primaryUser` for `userLaunchAgents` to
          # work
          users.users.stephen = {
            name = "stephen";
            home = "/Users/stephen";
          };
          system.primaryUser = "stephen";

          # Set up environment stuff
          environment = {
            userLaunchAgents."com.stephencottontail.fontsrv.plist" = {
              enable = true;
              text = ''
                <?xml version="1.0" encoding="UTF-8"?>
                <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST" 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
                <plist version="1.0">
                <dict>
                  <key>Label</key>
                  <string>com.stephencottontail.fontsrv</string>
                  <key>EnvironmentVariables</key>
                  <dict>
                    <key>PATH</key>
                    <string>/bin:/usr/bin:/usr/local/bin:${pkgs.plan9port}/plan9/bin</string>
                  </dict>
                  <key>Program</key>
                  <string>${fontsrv}</string>
                  <key>RunAtLoad</key>
                  <true/>
                  <key>StandardOutPath</key>
                  <string>/tmp/fontsrv.out</string>
                  <key>StandardErrPath</key>
                  <string>/tmp/fontsrv.err</string>
                </dict>
                </plist>
              '';
            };
            variables = {
              PLAN9 = "${pkgs.plan9port}/plan9";
              SHELL = "rc";
            };
          };

          # ZSH
          programs.zsh = {
            enable = true;
            promptInit = ''
              export PROMPT="%m> "
            '';
          };

<<<<<<< Updated upstream
          # Extra environment variables
          environment.variables = {
            PLAN9 = "${pkgs.plan9port}/plan9";
          };

          # Extra fonts
          fonts.packages = [
            pkgs.cm_unicode
          ];

=======
>>>>>>> Stashed changes
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
      # $ sudo darwin-rebuild build --flake .
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
