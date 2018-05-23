{ mkDerivation, aeson, aeson-casing, aeson-pretty, async, base
, bytestring, data-default, file-embed, generics-sop, hedgehog
, lens, lens-aeson, memory, mtl, path, path-io, process
, safe-exceptions, stdenv, text, transformers, unordered-containers
, vector, web3
}:
mkDerivation {
  pname = "melon";
  version = "0.1.0.0";
  src = ../../melon;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-casing base bytestring data-default file-embed
    generics-sop hedgehog lens lens-aeson memory path path-io process
    safe-exceptions text transformers web3
  ];
  executableHaskellDepends = [
    aeson aeson-pretty async base bytestring data-default hedgehog lens
    lens-aeson mtl path path-io unordered-containers vector web3
  ];
  description = "Property based random tests for Melon smart-contract";
  license = stdenv.lib.licenses.gpl3;
}
