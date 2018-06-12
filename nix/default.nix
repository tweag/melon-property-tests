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
          rev = "54f47cfd0098682d56e257e19428d2f9d341c2c8";
          sha256 = "0zxisq36yp4w78g4hzi6r5y5m4k3d3drfyscxiqbgb4xa7cig3ip";
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
