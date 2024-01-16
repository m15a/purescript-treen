{
  description = "Print anything as tree-like format.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url  = "github:numtide/flake-utils";
    purescript-overlay = {
      url = "github:thomashoneyman/purescript-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, purescript-overlay }:
  flake-utils.lib.eachDefaultSystem (system:
  let
    pkgs = import nixpkgs {
      inherit system;
      overlays = [
        purescript-overlay.overlays.default
      ];
    };
  in
  {
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
