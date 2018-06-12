{ mkDerivation, aeson-pretty, base, bytestring, data-default
, decimal-arithmetic, file-embed, generics-sop, hedgehog
, http-client, lens, lens-aeson, memory, mmorph, mtl
, optparse-applicative, path, path-io, safe-exceptions, stdenv
, template-haskell, text, transformers, unordered-containers, web3
, wreq
}:
mkDerivation {
  pname = "melon";
  version = "0.1.0.0";
  src = ../../melon;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson-pretty base bytestring data-default decimal-arithmetic
    file-embed generics-sop hedgehog http-client lens lens-aeson memory
    mmorph mtl path path-io safe-exceptions template-haskell text
    transformers unordered-containers web3 wreq
  ];
  executableHaskellDepends = [
    base hedgehog lens optparse-applicative
  ];
  description = "Property based random tests for Melon smart-contract";
  license = stdenv.lib.licenses.bsd3;
}
