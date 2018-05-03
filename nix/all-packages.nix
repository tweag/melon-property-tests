# Nixpkgs overlay - overrides and additional packages for this repository
self: super:

with super;

{
  fetchJSON = callPackage ./build-support/fetch-json.nix {};

  node2nix-src = self.fetchJSON ./node2nix-src.json;
  node2nix = (callPackage self.node2nix-src {}).package;
}
