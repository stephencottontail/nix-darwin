let
  files = builtins.filter
    (f: f != "default.nix")
    (builtins.attrNames (builtins.readDir ./.));
in
map (f: import ./${f}) files
