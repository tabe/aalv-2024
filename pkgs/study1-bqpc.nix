{ pkgs, stdenv, custom-R, study1-data }:
stdenv.mkDerivation {
  pname = "xbic-study1-bqpc";
  version = "2023.07.07";

  src = ../study1;

  nativeBuildInputs = [ custom-R ];

  configurePhase = ''
    tar xf ${study1-data}/study1-data.tar.gz
  '';

  makeFlags = [ "study1-bqpc.tar.gz" ];

  installPhase = ''
    install -d $out
    install study1-bqpc.tar.gz $out/
  '';

  enableParallelBuilding = true;
}
