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
  ];

  buildInputs = [
    haskellPackages.ghc
  ];
}
