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
    overlay = import ./overlay.nix;
  in
  flake-utils.lib.eachDefaultSystem (system:
  let
    pkgs = import nixpkgs {
      inherit system;
      overlays = [
        purs-overlay.overlays.default
        mk-spago-deriv.overlays.default
        overlay
      ];
    };
  in
  {
    overlays.default = overlay;

    packages = rec {
      inherit (pkgs) treen;
      default = treen;
    };

    apps = rec {
      treen = flake-utils.lib.mkApp {
        drv = self.packages.treen;
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
  });
}
