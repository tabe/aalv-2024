{ pkgs, stdenv, custom-R, study0-data }:
stdenv.mkDerivation {
  pname = "xbic-study0-gqpc";
  version = "2024.01.15";

  src = ../study0;

  nativeBuildInputs = [ custom-R ];

  configurePhase = ''
    tar xf ${study0-data}/study0-data.tar.gz
  '';

  makeFlags = [ "study0-gqpc.tar.gz" ];

  ## to avoid overcommitting CPU cores in the code of tseries
  preBuild = ''
    export OMP_NUM_THREADS=1
  '';

  installPhase = ''
    install -d $out
    install study0-gqpc.tar.gz $out/
  '';

  enableParallelBuilding = true;
}
