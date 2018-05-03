{ mkDerivation, base, hedgehog, path, path-io, process
, safe-exceptions, stdenv, web3
}:
mkDerivation {
  pname = "melon-property-tests";
  version = "0.1.0.0";
  src = ../../melon-property-tests;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base path process safe-exceptions web3 ];
  executableHaskellDepends = [ base hedgehog path path-io process ];
  description = "Property based random tests for Melon smart-contract";
  license = stdenv.lib.licenses.gpl3;
}
