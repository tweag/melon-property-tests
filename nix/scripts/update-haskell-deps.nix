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

[ -d nix/melon ] || mkdir nix/melon
cd nix/melon
${cabal2nix}/bin/cabal2nix ../../melon \
  > default.nix
cd - >/dev/null

[ -d nix/hs-web3 ] || mkdir nix/hs-web3
cd nix/hs-web3
${cabal2nix}/bin/cabal2nix ../../hs-web3 \
  > default.nix
cd - >/dev/null
''
