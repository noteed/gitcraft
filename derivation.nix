{ mkDerivation, base, mtl, SHA, stdenv }:
mkDerivation {
  pname = "gitcraft";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [ base mtl SHA ];
  testHaskellDepends = [
    base
  ];
  homepage = "https://noteed.com/gitcraft";
  description = "Craft Git commit diagrams";
  license = stdenv.lib.licenses.bsd2;
}
