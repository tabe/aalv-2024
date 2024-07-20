{ pkgs, stdenv, custom-R, study1-data }:
stdenv.mkDerivation {
  pname = "xbic-study1-gqpc";
  version = "2023.07.07";

  src = ../study1;

  nativeBuildInputs = [ custom-R ];

  configurePhase = ''
    tar xf ${study1-data}/study1-data.tar.gz
  '';

  makeFlags = [ "study1-gqpc.tar.gz" ];

  ## to avoid overcommitting CPU cores in the code of tseries
  preBuild = ''
    export OMP_NUM_THREADS=1
  '';

  installPhase = ''
    install -d $out
    install study1-gqpc.tar.gz $out/
  '';

  enableParallelBuilding = true;
}
