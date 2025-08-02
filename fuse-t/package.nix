{
  lib,
  stdenv,
  fetchurl,
  xar,
  cpio,
}:

let
  pname = "fuse-t";
  version = "1.0.47";
in
stdenv.mkDerivation (finalAttrs: {
  inherit pname version;

  src = fetchurl {
    url = "https://github.com/macos-fuse-t/fuse-t/releases/download/${version}/fuse-t-macos-installer-${version}.pkg";
    sha256 = "sha256:1wq70kng5lma9y431zci4yx11i5qpj7ywapd0rig48mxj9gin4mh";
  };

  nativeBuildInputs = [
    xar
    cpio
  ];

  unpackPhase = ''
    xar -xf $src
    zcat < fuse-t.pkg/Payload | cpio -i
  '';

  dontPatch = true;
  dontConfigure = true;
  dontBuild = true;

  # empty - files are copied as part of an activation script
  installPhase = ''
    runHook preInstall
    cp -R . $out
    runHook postInstall
  '';
})
