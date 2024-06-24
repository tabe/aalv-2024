{
  description = "Cross-bicoherence studies";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-23.05;

  outputs = { self, nixpkgs }: let

    allSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];

    forAllSystems = f: nixpkgs.lib.genAttrs allSystems (system: f (import nixpkgs { inherit system; }));

    xbic-packages = pkgs: with pkgs; let
      qpcstudies = callPackage ./pkgs/qpcstudies.nix {};
      custom-R = callPackage ./pkgs/custom-R.nix { inherit qpcstudies; };
      study0-data = callPackage ./pkgs/study0-data.nix { inherit custom-R; };
      study0-bqpc = callPackage ./pkgs/study0-bqpc.nix { inherit custom-R study0-data; };
      study0-gqpc = callPackage ./pkgs/study0-gqpc.nix { inherit custom-R study0-data; };
      study0 = callPackage ./pkgs/study0.nix { inherit custom-R study0-data study0-bqpc study0-gqpc; };
      study1-data = callPackage ./pkgs/study1-data.nix { inherit custom-R; };
      study1-bqpc = callPackage ./pkgs/study1-bqpc.nix { inherit custom-R study1-data; };
      study1-gqpc = callPackage ./pkgs/study1-gqpc.nix { inherit custom-R study1-data; };
      study1 = callPackage ./pkgs/study1.nix { inherit custom-R study1-data study1-bqpc study1-gqpc; };
      xbic-conf = callPackage ./pkgs/xbic-conf.nix {};
    in {
      inherit study0-data;
      inherit study1-data;
      inherit qpcstudies;
      inherit study0-bqpc study0-gqpc study0;
      inherit study1-bqpc study1-gqpc study1;
      default = qpcstudies;
    };

    xbic-shells = pkgs: with pkgs; let
      qpcstudies = callPackage ./pkgs/qpcstudies.nix {};
      custom-R = callPackage ./pkgs/custom-R.nix { inherit qpcstudies; };
      default-shell = mkShell {
        packages = [ custom-R ];
      };
    in {
      default = default-shell;
    };

  in {

    packages  = forAllSystems xbic-packages;

    devShells = forAllSystems xbic-shells;

  };
}
