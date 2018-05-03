{ mkDerivation, aeson, async, base, basement, bytestring, cereal
, cryptonite, data-default, exceptions, fetchgit, generics-sop
, hspec, hspec-contrib, hspec-discover, hspec-expectations
, http-client, machines, memory, mtl, parsec, split, stdenv, stm
, tagged, template-haskell, text, time, transformers
}:
mkDerivation {
  pname = "web3";
  version = "0.7.1.0";
  src = fetchgit {
    url = "https://github.com/airalab/hs-web3.git";
    sha256 = "0b2ixxybpm44nkfqkxb9y55gfflnfkksl9zsq2zs54ir69bp271j";
    rev = "3291f4038ca7e71852e2729b85483e10a2fa5e3a";
  };
  libraryHaskellDepends = [
    aeson async base basement bytestring cereal cryptonite data-default
    exceptions generics-sop http-client machines memory mtl parsec
    tagged template-haskell text transformers
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
