let
  pkgs = import ./nix {};
in

with pkgs;

mkShell {
  inputsFrom = [
    smart-contracts.shell
  ];

  nativeBuildInputs = [
    cabal-install
    cabal2nix
    git
    node2nix
    parity
    stack

    scripts.update-node-deps
    scripts.update-haskell-deps
    scripts.build-smart-contracts
  ];

  buildInputs = [
    haskellPackages.ghc
  ];
}
