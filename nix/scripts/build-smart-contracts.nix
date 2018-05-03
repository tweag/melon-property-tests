{ writeShellScriptBin, bash, node2nix }:
writeShellScriptBin "build-smart-contracts" ''
#!${bash}/bin/bash
#
# Build the smart-contracts repository according to its README instructions.
#
set -eEuo pipefail

err_msg () {
  echo "Command must be executed in repository root." >&2
}

trap err_msg ERR

cd smart-contracts
npm install
npm run compile
''
