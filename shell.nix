let
  pkgs = import ./nix {};
in

with pkgs;

mkShell {
  inputsFrom = [
    smart-contracts.shell
    haskellPackages.melon.env
  ];

  nativeBuildInputs = [
    cabal-install
    cabal2nix
    git
    node2nix
    parity

    scripts.update-node-deps
    scripts.update-haskell-deps
    scripts.build-smart-contracts
  ];

  buildInputs = [
    zlib
  ];
}
