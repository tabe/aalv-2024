{ pkgs, stdenv, custom-R }:
stdenv.mkDerivation {
  pname = "xbic-study0-data";
  version = "2024.01.15";

  src = ../study0-data;

  nativeBuildInputs = [ custom-R ];

  makeFlags = [ "study0-data.tar.gz" ];

  installPhase = ''
    install -d $out
    install study0-data.tar.gz $out/
  '';

  enableParallelBuilding = true;
}
