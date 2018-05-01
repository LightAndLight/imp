{ mkDerivation, base, llvm-hs, llvm-hs-pretty, llvm-hs-pure, mtl
, stdenv, text, trifecta
}:
mkDerivation {
  pname = "imp";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base llvm-hs llvm-hs-pretty llvm-hs-pure mtl text trifecta
  ];
  license = stdenv.lib.licenses.bsd3;
}
