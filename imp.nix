{ mkDerivation, base, stdenv, trifecta }:
mkDerivation {
  pname = "imp";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base trifecta ];
  license = stdenv.lib.licenses.bsd3;
}
