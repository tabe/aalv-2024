{ pkgs, stdenv, custom-R, study0-data, study0-bqpc, study0-gqpc }:
stdenv.mkDerivation {
  pname = "xbic-study0";
  version = "2024.01.15";

  src = ../study0;

  nativeBuildInputs = [ custom-R study0-bqpc study0-gqpc ];

  configurePhase = ''
    tar xf ${study0-data}
    tar xf ${study0-bqpc}/study0-bqpc.tar.gz
    tar xf ${study0-gqpc}/study0-gqpc.tar.gz
  '';

  makeFlags = [ "study0.tar.gz" ];

  installPhase = ''
    install -d $out
    install study0.tar.gz $out/
  '';

  enableParallelBuilding = true;
}
