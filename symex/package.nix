{
  trivialBuild,
  fetchFromGitHub,
  lib,
  pkgs,
}:

trivialBuild rec {
  pname = "symex";
  version = "2.0";
  
  src = fetchFromGitHub {
    owner = "drym-org";
    repo = "symex.el";
    rev = "64c863d84355f1d4618c5f77f14996d37f7bf2d4";
    hash = "sha256-3bhtKrpleLmEkKNn9jI29WNUA3qTFt3XdCYXWruTG0g=";
  };

  packageRequires = [
    pkgs.emacsPackages.lithium
    pkgs.emacsPackages.pubsub
    pkgs.emacsPackages.mantra
    pkgs.emacsPackages.virtual-ring
    pkgs.emacsPackages.repeat-ring
  ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/emacs/site-lisp/symex
    mkdir -p $out/share/emacs/site-lisp/symex-core
    mkdir -p $out/share/emacs/site-lisp/symex-ide

    cp symex/*.el $out/share/emacs/site-lisp/symex
    cp symex-core/*.el $out/share/emacs/site-lisp/symex-core
    cp symex-ide/*.el $out/share/emacs/site-lisp/symex-ide

    # Skip postInstall because native compilation fails due
    # to missing `paredit` and I could probably fix it but
    # I don't have time to right now
  '';

  meta = with lib; {
    description = "Fancy semantic editing mode for cool kids";
    homepage = "https://github.com/drym-org/symex.el";
    license = licenses.publicDomain;
    platforms = platforms.all;
  };
}

