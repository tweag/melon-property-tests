/**
 * This is the entry-point for all nix execution in this project.
 */
{ nixpkgsSrc ? ./nixpkgs }:
import (import ./nixpkgs) {
  # Makes the config pure as well. See <nixpkgs>/top-level/impure.nix:
  config = {
    allowBroken = true;
  };
  overlays = [
    # all local packages are defined there:
    (import ./all-packages.nix)
    # dapp-hub dependencies are included here:
    ( self: super:
      let
        spec = {
          owner = "dapphub";
          repo = "nixpkgs-dapphub";
          rev = "8eb683629c304a64658f77277eceb5f553bf95ba";
          sha256 = "1hp5xp7dp4igwy4i92r9qf5lc03s3r0rgyiddkfy3hhwxr8n1g30";
        };
        pkgs' = import (import ./nixpkgs) { config = {}; overlays = []; };
        src = pkgs'.fetchFromGitHub {
          inherit (spec) owner repo rev sha256;
          fetchSubmodules = true;
        };
        release = import "${src}/release.nix" {
          pkgsPath = self.path;
          system = builtins.currentSystem;
        };
      in
      { inherit (release.dapphub.linux.stable) dapp seth hevm evmdis; }
    )
  ];
}
