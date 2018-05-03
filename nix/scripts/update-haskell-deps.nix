{ writeShellScriptBin, bash, cabal2nix }:
writeShellScriptBin "update-haskell-deps" ''
#!${bash}/bin/bash
#
# Run cabal2nix to update the Haskell package collection
#
set -eEuo pipefail

err_msg () {
  echo "Command must be executed in repository root." >&2
}

trap err_msg ERR

[ -d nix/melon-property-tests ] || mkdir nix/melon-property-tests
cd nix/melon-property-tests
${cabal2nix}/bin/cabal2nix ../../melon-property-tests \
  > default.nix
''
