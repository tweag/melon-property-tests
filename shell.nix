let
  pkgs = import ./nix {};
in

with pkgs;

let
  ghcWithDependenciesAndSystemDependencies =
    let
      internalHaskellPackages = [ "melon" "web3" ];
      haskellPackages = pkgs.haskellPackages;
      # Check that a Haskell package is external to this repository.
      isExternalPackage = x:
        lib.all (internalName: x.pname or "" != internalName)
        internalHaskellPackages
      ;
      # Determine if a package is a Haskell package or not.  Stolen from:
      # <nixpkgs/pkgs/development/haskell-modules/generic-builder.nix>
      isHaskellPkg = x: (x ? pname) && (x ? version) && (x ? env);
      isSystemPkg = x: !isHaskellPkg x;

      allDependencies =
        lib.concatMap
          ( pname:
            let pkg = haskellPackages."${pname}"; in
            lib.concatLists [
              pkg.buildInputs
              pkg.nativeBuildInputs
              pkg.propagatedBuildInputs
              pkg.propagatedNativeBuildInputs
            ]
          )
          internalHaskellPackages
      ;
      haskellDependencies =
        builtins.filter
          (x: isHaskellPkg x && isExternalPackage x)
          allDependencies
      ;
      systemDependencies = builtins.filter isSystemPkg allDependencies;
      ghcWithDependencies = haskellPackages.ghcWithPackages
        (ps: with ps; haskellDependencies)
      ;
    in
    { inherit ghcWithDependencies systemDependencies; }
  ;
in with ghcWithDependenciesAndSystemDependencies;

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

    scripts.update-node-deps
    scripts.update-haskell-deps
    scripts.build-smart-contracts
  ];

  buildInputs = [
    ghcWithDependencies
    systemDependencies
    zlib
  ];

  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
