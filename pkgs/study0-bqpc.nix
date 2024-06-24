{ pkgs, stdenv, custom-R, study0-data }:
stdenv.mkDerivation {
  pname = "xbic-study0-bqpc";
  version = "2024.01.15";

  src = ../study0;

  nativeBuildInputs = [ custom-R ];

  configurePhase = ''
    tar xf ${study0-data}/study0-data.tar.gz
  '';

  makeFlags = [ "study0-bqpc.tar.gz" ];

  installPhase = ''
    install -d $out
    install study0-bqpc.tar.gz $out/
  '';

  enableParallelBuilding = true;
}
