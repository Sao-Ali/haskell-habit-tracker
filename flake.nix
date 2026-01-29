{
  description = "haskell-habit: dev shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        hs = pkgs.haskellPackages;
      in {
        devShells.default = pkgs.mkShell {
          packages = [
            hs.ghc
            hs.cabal-install
            hs.haskell-language-server
            hs.aeson
            hs.time
            hs.containers
          ];
        };
      });
}
