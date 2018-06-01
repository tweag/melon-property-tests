# Nixpkgs overlay - overrides and additional packages for this repository
self: super:

with super;

{
  fetchJSON = callPackage ./build-support/fetch-json.nix {};

  node2nix-src = self.fetchJSON ./node2nix-src.json;
  node2nix = (callPackage self.node2nix-src {}).package;

  haskellPackages = callPackage ./haskell {};

  smart-contracts =
    let
      smart-contracts = callPackage ./smart-contracts {
        nodejs = self.nodejs-8_x;
      };
    in
    smart-contracts // {
      package = smart-contracts.package.overrideAttrs (attrs: {
        nativeBuildInputs = attrs.nativeBuildInputs or [] ++ [ self.git ];
      });
    }
  ;

  scripts = {
    update-node-deps = callPackage ./scripts/update-node-deps.nix {};
    update-haskell-deps = callPackage ./scripts/update-haskell-deps.nix {};
    build-smart-contracts = callPackage ./scripts/build-smart-contracts.nix {};
  };
}
