{ mkDerivation, base, bytestring, mtl, SHA, stdenv }:
mkDerivation {
  pname = "gitcraft";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base bytestring mtl SHA ];
  homepage = "http://noteed.com/gitcraft";
  description = "Craft Git commit diagrams";
  license = stdenv.lib.licenses.bsd2;
}
