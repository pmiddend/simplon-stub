# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0

{
  description = "simplon-stub-hs";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ ];
          };

          haskellPackages = pkgs.haskellPackages.override {
            overrides = self: super: { };
          };

          packageName = "simplon-stub-hs";
        in
        {
          packages.${packageName} =
            haskellPackages.callCabal2nix packageName self
              {
                scotty = haskellPackages.scotty_0_22;
              };

          packages.default = self.packages.${system}.${packageName};

          defaultPackage = self.packages.${system}.default;

          devShells.default =
            pkgs.mkShell {
              buildInputs = with pkgs; [
                haskellPackages.haskell-language-server # you must build it with your ghc to work
                cabal-install
                ghcid
                haskellPackages.hlint
                haskellPackages.apply-refact

              ];
              inputsFrom = [ self.packages.${system}.simplon-stub-hs.env ];
            };
          devShell = self.devShells.${system}.default;
        });
}
