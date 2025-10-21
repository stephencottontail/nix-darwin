{
  lib,
  pkgs,
  gccStdenv,
  ncurses,
  pkg-config,
  fetchzip
}:

gccStdenv.mkDerivation {
  pname = "ibiblio-teco";
  version = "";

  src = fetchzip {
    url = "https://www.ibiblio.org/pub/linux/apps/editors/tty/teco.tar.gz";
    hash = "sha256-NFqF8vUkJb6dRCgXDYrwvM3VgShVvqGFYdmlRkyVfYs=";
    stripRoot = false;
  };

  nativeBuildInputs = [
    ncurses
    pkg-config
  ];

  patches = [
    ./ncurses.patch
    ./isalpha.patch
    ./no-common.patch
    ./void-functions.patch
  ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    mkdir -p $out/share/doc
    mkdir -p $out/share/man/man1

    cp te $out/bin
    cp te.1 $out/share/man/man1
    cp sample.tecorc $out/share/doc
    cp sample.tecorc2 $out/share/doc
    cp READ.ME $out/share/doc

    runHook postInstall
  '';
}
