{ mkDerivation, aeson, aeson-casing, aeson-pretty, base, bytestring
, data-default, hedgehog, lens, lens-aeson, path, path-io, process
, safe-exceptions, stdenv, unordered-containers, vector, web3
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
    aeson aeson-pretty base bytestring data-default hedgehog lens
    lens-aeson path path-io unordered-containers vector web3
  ];
  description = "Property based random tests for Melon smart-contract";
  license = stdenv.lib.licenses.gpl3;
}
