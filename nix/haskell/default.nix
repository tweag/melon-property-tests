{ haskell }:
# Build a new overlay with our own packages
haskell.packages.ghc822.extend (self: super: {
  melon-property-tests = super.callPackage ../melon-property-tests {};
  web3 = haskell.lib.dontCheck (super.callPackage ../hs-web3 {});
})
