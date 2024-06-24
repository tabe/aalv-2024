{ pkgs, stdenv, custom-R, study1-data, study1-bqpc, study1-gqpc }:
stdenv.mkDerivation {
  pname = "xbic-study1";
  version = "2023.06.24";

  src = ../study1;

  nativeBuildInputs = [ custom-R study1-bqpc study1-gqpc ];

  configurePhase = ''
    tar xf ${study1-data}
    tar xf ${study1-bqpc}/study1-bqpc.tar.gz
    tar xf ${study1-gqpc}/study1-gqpc.tar.gz
  '';

  makeFlags = [ "study1.tar.gz" ];

  installPhase = ''
    install -d $out
    install study1.tar.gz $out/
  '';

  enableParallelBuilding = true;
}
