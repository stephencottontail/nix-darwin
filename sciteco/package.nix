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
  version = "2.5.2";

  src = fetchgit {
    url             = "git://git.fmsbw.de/sciteco/";
    rev             = "cdcbab03107fd74855cfd201161bd116b2eebba8";
    hash            = "sha256-eqmDIpnOItn0CiRNeUQ3D8LQX7FFahd6wNaEDwlo+nc=";
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

