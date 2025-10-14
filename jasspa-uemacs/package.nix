{
  lib,
  gccStdenv,
  fetchFromGitHub,
  bash,
  ncurses,
  pkgs
}:

gccStdenv.mkDerivation {
  pname = "jasspa-uemacs";
  version = "20250901";

  src = fetchFromGitHub {
    owner = "bjasspa";
    repo = "jasspa";
    rev = "bbb7828c7db86510ab8ab9a6719c38fc2969b6a3";
    hash = "sha256-rm2AvbeRZcNYAu+WEXZTDv1R8yPRU9EM+ZwHOjM/f6A=";
  };

  buildInputs = with pkgs; [
    bash
    ncurses
    darwin.DarwinTools
  ];

  buildPhase = ''
    runHook preBuild

    cd microemacs/src
    ./build.sh -t c -64 -v CCFLAGSR="-Wno-implicit-function-declaration" -v CONSOLE_LIBS="-lncurses" -v CONSOLE_DEFS="-D_USE_NCURSES"

    runHook postBuild
  '';

  preInstall = ''
    cd ../mesingle
    export PATH=`pwd`/../bin/macos26-apple64-cc14:$PATH
    bash mesgen.sh \
      -p ../bin/macos26-apple64-cc14/mec -o mesc
  '';
    
  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    mkdir -p $out/share/bin
    cp ./mesc $out/share/bin
    cp -r ../macros $out/share/macros
    ln -sf $out/share/bin/mesc $out/bin/mesc

    runHook postInstall
  '';
}
