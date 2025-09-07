{
  pkgs,
  lib,
}:

pkgs.buildGoModule rec {
  pname = "acme-lsp";
  version = "0.11.0";

  src = pkgs.fetchFromGitHub {
    owner = "stephencottontail";
    repo = "acme-lsp";
    rev = "7cf0062884ebc36877c3f3c17f3ae74c6baa7192";
    hash =  "sha256-C5j+g2Wsrvrkg62dMb9NMenN+zFfDxhVqxnYK0UQV7Y=";
  };

  # Tests turow an error, possibly due to missing FUSE setup
  doCheck = false;
  doInstallCheck = false;

  # If your Go module does not vendor dependencies, and you want Nix to fetch them
  vendorHash = null; # Set to null if you are not vendoring dependencies and relying on go mod download

  # Optional: Define build flags or other arguments for the Go build process
  # buildFlags = [ "-ldflags" "-s -w" ];
}
