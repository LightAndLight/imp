{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:

let

  inherit (nixpkgs) pkgs;

  f = import ./imp.nix;

  haskellPackages =
    (if compiler == "default"
       then pkgs.haskellPackages
       else pkgs.haskell.packages.${compiler}).override {
       overrides = self: super: {
         llvm-hs-pure = super.callPackage ./nix/llvm-hs-pure.nix {};
         llvm-hs = self.callPackage ./nix/llvm-hs.nix {
           llvm-config = pkgs.llvm_5;
         };
         llvm-hs-pretty = pkgs.haskell.lib.dontCheck super.llvm-hs-pretty;
       };
     };


  drv = haskellPackages.callPackage f {};

in

  drv
