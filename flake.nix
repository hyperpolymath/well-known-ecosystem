# SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
#
# well-known-ecosystem - Nix Flake (fallback for non-Guix systems)
# Primary package manager: Guix (see guix.scm)
{
  description = "Well-known URI ecosystem management with RSR compliance";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        packages.default = pkgs.stdenv.mkDerivation {
          pname = "well-known-ecosystem";
          version = "0.1.0";
          src = ./.;

          meta = with pkgs.lib; {
            description = "Guix channel/infrastructure - part of the RSR ecosystem";
            homepage = "https://github.com/hyperpolymath/well-known-ecosystem";
            license = with licenses; [ mit agpl3Plus ];
            maintainers = [ ];
            platforms = platforms.all;
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            guile
            just
          ];

          shellHook = ''
            echo "well-known-ecosystem development shell"
            echo "Primary package manager: Guix (guix shell -D -f guix.scm)"
            echo "This Nix flake is a fallback for non-Guix systems."
          '';
        };
      }
    );
}
