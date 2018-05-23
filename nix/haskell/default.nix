{ haskell }:
# Build a new overlay with our own packages
haskell.packages.ghc822.extend (self: super: {
  melon = super.callPackage ../melon {};
  web3 = super.callPackage ../hs-web3 {};
})
