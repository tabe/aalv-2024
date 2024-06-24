{ pkgs, stdenv, custom-R }:
stdenv.mkDerivation {
  pname = "xbic-study1-data";
  version = "2023.06.24";

  src = ../study1-data;

  nativeBuildInputs = [ custom-R ];

  makeFlags = [ "study1-data.tar.gz" ];

  installPhase = ''
    install -d $out
    install study1-data.tar.gz $out/
  '';

  enableParallelBuilding = true;
}
