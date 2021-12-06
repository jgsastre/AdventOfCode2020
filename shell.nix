{ nixpkgs ? import <nixpkgs> {} }:
let
in
  nixpkgs.mkShell {
    buildInputs = [
      nixpkgs.haskell-language-server
      nixpkgs.haskellPackages.cabal-install
      nixpkgs.haskellPackages.regex-compat
      nixpkgs.ghc
      nixpkgs.ghcid
      nixpkgs.hlint
      nixpkgs.ispell
    ];
  }
