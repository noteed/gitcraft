{ mkDerivation, base, lib, bytestring, mtl, SHA }:
mkDerivation {
  pname = "gitcraft";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base bytestring mtl SHA ];
  homepage = "http://noteed.com/gitcraft";
  description = "Craft Git commit diagrams";
  license = lib.licenses.bsd2;
}
