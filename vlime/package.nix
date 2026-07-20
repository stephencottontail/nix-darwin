{ pkgs }:

pkgs.vimUtils.buildVimPlugin {
  pname = "vlime";
  version = "0.4.0";

  src = pkgs.fetchFromGitHub {
    owner = "vlime";
    repo = "vlime";
    rev = "e276e9a6f37d2699a3caa63be19314f5a19a1481";
    hash = "sha256-tCqN80lgj11ggzGmuGF077oqL5ByjUp6jVmRUTrIWJA=";
  };
}

