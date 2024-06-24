{ lib, pkgs, stdenv }:
let
  fixup-libiconv = self: super: {
    propagatedBuildInputs = super.propagatedBuildInputs ++ lib.optional stdenv.isDarwin pkgs.libiconv;
  };

  fixed-rPackages = pkgs.rPackages.override {
    overrides = {
      # https://discourse.nixos.org/t/guidance-to-get-r-package-sf-built-on-macbook-aarch64/22114
      lmtest = pkgs.rPackages.lmtest.overrideAttrs fixup-libiconv;
      multitaper = pkgs.rPackages.multitaper.overrideAttrs fixup-libiconv;
      strucchange = pkgs.rPackages.strucchange.overrideAttrs fixup-libiconv;
      tseries = pkgs.rPackages.tseries.overrideAttrs fixup-libiconv;
      urca = pkgs.rPackages.urca.overrideAttrs fixup-libiconv;
      # https://github.com/r-lib/textshaping/issues/19
      # https://discourse.nixos.org/t/rpackages-textshaping-not-working-on-osx-m1/25550
    };
  };
in
fixed-rPackages.buildRPackage {
  name = "qpcstudies";
  version = "2024.01.15";

  src = ../qpcstudies;

  propagatedBuildInputs = with fixed-rPackages; [
    cowplot
    devtools
    dplyr
    ggplot2
    ggpp
    grangers
    magrittr
    multitaper
    psych
    purrr
    qvalue
    readr
    rhosa
    testthat
    tibble
    tidyr
    tseries
    vars
  ];
}
