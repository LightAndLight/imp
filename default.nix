{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:

let

  inherit (nixpkgs) pkgs;

  f = import ./imp.nix;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  drv
