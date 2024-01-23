{
  description = "Print trees from anything.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    purs-overlay = {
      url = "github:thomashoneyman/purescript-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    mk-spago-deriv = {
      url = "github:jeslie0/mkSpagoDerivation";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.ps-overlay.follows = "purs-overlay";
    };
  };

  outputs = { self, nixpkgs, flake-utils, purs-overlay, mk-spago-deriv, ... }:
  let
    treen-overlay = import ./overlay.nix;
  in
  {
    overlays = rec {
      treen = treen-overlay;
      default = treen;
    };
  } // (flake-utils.lib.eachDefaultSystem (system:
  let
    pkgs = import nixpkgs {
      inherit system;
      overlays = [
        purs-overlay.overlays.default
        mk-spago-deriv.overlays.default
        treen-overlay
      ];
    };
  in
  {
    packages = rec {
      inherit (pkgs) treen;
      default = treen;
    };

    apps = rec {
      treen = flake-utils.lib.mkApp {
        drv = self.packages.${system}.treen;
        name = "treen";
      };
      default = treen;
    };

    devShells.default = pkgs.mkShell {
      buildInputs = with pkgs; [
        esbuild
        nodejs
        purs
        purs-backend-es
        spago-unstable
        purs-tidy
      ];
    };
  }));
}
