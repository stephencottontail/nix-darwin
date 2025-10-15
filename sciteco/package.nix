{
  lib,
  gccStdenv,
  fetchgit,
  gcc,
  autoconf,
  automake,
  pkg-config,
  libtool,
  ncurses,
  groff,
  doxygen,
  glib,
  pkgs
}:

gccStdenv.mkDerivation {
  pname = "sciteco";
  version = "2.4.0";

  src = fetchgit {
    url             = "git://git.fmsbw.de/sciteco/";
    rev             = "cb972565f3050e567e6781b4abea82932d8d7310";
    hash            = "sha256-l0evFt7KwF0C8yN1m+5hWRDa4KMPDH8FNP95hBjzhh4=";
    fetchSubmodules = true;
  };

  nativeBuildInputs = with pkgs; [
    gcc
    autoconf
    automake
    pkg-config
    libtool
    ncurses
    groff
    doxygen
    glib
  ];

  configurePhase = ''
    autoreconf -i
    ./configure --prefix=$out
  '';

  preBuild = ''
    sed -i.bak 's/libtool/\/usr\/bin\/libtool/' contrib/lexilla/src/makefile
  '';

  buildPhase = ''
    runHook preBuild

    make

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    make install

    runHook postInstall
  '';
}
