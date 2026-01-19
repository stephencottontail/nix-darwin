{
  stdenv,
  fetchurl,
  bzip2,
  gnutar
}:

stdenv.mkDerivation rec {
  pname = "vimr";
  version = "v0.59.3";
  timestamp="20251222.155246";

  src = fetchurl {
    url = "https://github.com/qvacua/${pname}/releases/download/${version}-${timestamp}/VimR-${version}.tar.bz2";
    name ="VimR.tar.bz2";
    hash = "sha256-JNEuXbR0jduDqXtxogayl7RaDcCleiV+RGbj5LA8eps=";
  };

  # note: after Nix unpacks the source it assumes there's only one top-level
  # folder and calls that `sourceRoot` which becomes `.` for all phases of
  # the build process. This is usually ok because most things are well-
  # behaved and unpack to a single folder which contains the actual source,
  # but it fails here because we're downloading an official release which
  # contains only `VimR.app`
  sourceRoot = ".";

  buildPhase = ''
    runHook preBuild

    mkdir -p $out/Applications
    mkdir -p $out/bin
    cp -R VimR.app $out/Applications
    cp VimR.app/Contents/Resources/vimr $out/bin

    runHook postBuild
  '';
}
