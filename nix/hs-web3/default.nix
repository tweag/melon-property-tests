{ mkDerivation, aeson, async, base, basement, bytestring, cereal
, cryptonite, data-default, exceptions, generics-sop, hashable
, hspec, hspec-contrib, hspec-discover, hspec-expectations
, http-client, machines, memory, mtl, parsec, relapse, secp256k1
, split, stdenv, stm, tagged, template-haskell, text, time
, transformers
}:
mkDerivation {
  pname = "web3";
  version = "0.7.3.0";
  src = ../../hs-web3;
  libraryHaskellDepends = [
    aeson async base basement bytestring cereal cryptonite data-default
    exceptions generics-sop hashable http-client machines memory mtl
    parsec relapse secp256k1 tagged template-haskell text transformers
  ];
  testHaskellDepends = [
    async base bytestring data-default generics-sop hspec hspec-contrib
    hspec-discover hspec-expectations memory split stm tagged text time
    transformers
  ];
  homepage = "https://github.com/airalab/hs-web3#readme";
  description = "Ethereum API for Haskell";
  license = stdenv.lib.licenses.bsd3;
}
