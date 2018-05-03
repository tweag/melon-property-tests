{ haskell }:
# Build a new overlay with our own packages
haskell.packages.ghc822.extend (self: super: {
  melon = super.callPackage ../melon {};
  web3 = haskell.lib.dontCheck (super.callPackage ../hs-web3 {});
})
