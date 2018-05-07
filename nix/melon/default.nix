{ mkDerivation, aeson, aeson-casing, base, bytestring, data-default
, hedgehog, lens, lens-aeson, path, path-io, process
, safe-exceptions, stdenv, web3
}:
mkDerivation {
  pname = "melon";
  version = "0.1.0.0";
  src = ../../melon;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-casing base bytestring lens lens-aeson path process
    safe-exceptions web3
  ];
  executableHaskellDepends = [
    base data-default hedgehog path path-io web3
  ];
  description = "Property based random tests for Melon smart-contract";
  license = stdenv.lib.licenses.gpl3;
}
