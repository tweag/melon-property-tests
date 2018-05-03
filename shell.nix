let
  pkgs = import ./nix {};
in

with pkgs;

mkShell {
  inputsFrom = [
  ];

  nativeBuildInputs = [
    git
    node2nix
    stack

    scripts.update-node-deps
  ];

  buildInputs = [
    haskellPackages.ghc
  ];
}
