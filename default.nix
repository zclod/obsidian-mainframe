{ mkDerivation, base, protolude, stdenv }:
mkDerivation {
  pname = "obsidian-mainframe";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base protolude ];
  executableHaskellDepends = [ base ];
  license = stdenv.lib.licenses.bsd3;
}
