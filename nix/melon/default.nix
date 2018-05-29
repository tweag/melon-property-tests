{ mkDerivation, aeson, aeson-casing, aeson-pretty, async, base
, bytestring, containers, cryptocompare, data-default
, decimal-arithmetic, file-embed, generics-sop, hedgehog, lens
, lens-aeson, memory, mmorph, mtl, path, path-io, process
, safe-exceptions, scientific, stdenv, template-haskell, text
, transformers, unordered-containers, vector, web3, wreq
}:
mkDerivation {
  pname = "melon";
  version = "0.1.0.0";
  src = ../../melon;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-casing aeson-pretty async base bytestring containers
    cryptocompare data-default decimal-arithmetic file-embed
    generics-sop hedgehog lens lens-aeson memory mmorph mtl path
    path-io process safe-exceptions scientific template-haskell text
    transformers unordered-containers web3 wreq
  ];
  executableHaskellDepends = [
    aeson aeson-pretty async base bytestring data-default hedgehog lens
    lens-aeson mtl path path-io unordered-containers vector web3
  ];
  description = "Property based random tests for Melon smart-contract";
  license = stdenv.lib.licenses.gpl3;
}
