let
  pkgs = import ./nix {};
in

with pkgs;

mkShell {
  inputsFrom = [
    smart-contracts.shell
  ];

  nativeBuildInputs = [
    git
    node2nix
    stack

    scripts.update-node-deps
    scripts.build-smart-contracts
  ];

  buildInputs = [
    haskellPackages.ghc
  ];
}
