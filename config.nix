{ stdenv }:

stdenv.mkDerivation rec {
  pname = "kaimacs-config";
  version = "2025-10-10";

  src = ./.;

  installPhase = ''
    mkdir -p $out
    cp -r {config,lib,templates,themes} $out
    cp *.el $out
  '';
}
