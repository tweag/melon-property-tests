{ writeShellScriptBin, bash, node2nix }:
writeShellScriptBin "update-node-deps" ''
#!${bash}/bin/bash
#
# Run node2nix to update the node package collection
#
set -eEuo pipefail

err_msg () {
  echo "Command must be executed in repository root." >&2
}

trap err_msg ERR

mkdir nix/smart-contracts
${node2nix}/bin/node2nix \
  -o nix/smart-contracts/node-packages.nix \
  -c nix/smart-contracts/default.nix \
  -e nix/smart-contracts/node-env.nix \
  -i smart-contracts/package.json \
  -l smart-contracts/package-lock.json \
  -d \
  -8
''
