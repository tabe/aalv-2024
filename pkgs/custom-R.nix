{ pkgs, qpcstudies }:
let
  packages = [ qpcstudies ] ++ (with pkgs.rPackages; [ tidyverse ]);
in
pkgs.rWrapper.override { inherit packages; }
