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

          haskellPackages = pkgs.haskellPackages.override
            {
              # This is a hack. We don't need hdf5 from hackage at all, but "pkgconfig-depends" in the cabal file
              # forces the usage of hdf5 somehow. So we map it to an existing, used package and everything works.
              overrides = self: super: {
                # hdf5 = pkgs.haskell.lib.markUnbroken super.hdf5;
                # hdf5 = pkgs.stdenv.mkDerivation { name = "myhdf5"; };
                # Looking weird? Rightfully so.
                # hdf5 = super.text;
              };
              # overrides = self: super: { };
            };

          hdf5-external-filter-plugins = with pkgs; stdenv.mkDerivation rec {
            pname = "HDF5-External-Filter-Plugins";
            version = "0.1.0";
            src = fetchFromGitHub {
              owner = "nexusformat";
              repo = pname;
              rev = "49e3b65eca772bca77af13ba047d8b577673afba";
              hash = "sha256-bEzfWdZuHmb0PDzCqy8Dey4tLtq+4coO0sT0GzqrTYI=";
            };

            patches = [
              (fetchpatch {
                url = "https://github.com/spanezz/HDF5-External-Filter-Plugins/commit/6b337fe36da97a3ef72354393687ce3386c0709d.patch";
                hash = "sha256-wnBEdL/MjEyRHPwaVtuhzY+DW1AFeaUQUmIXh+JaRHo=";
              })
            ];

            nativeBuildInputs = [ cmake ];
            buildInputs = [ hdf5 lz4 bzip2 ];

            cmakeFlags = [
              "-DENABLE_BITSHUFFLE_PLUGIN=yes"
              "-DENABLE_LZ4_PLUGIN=yes"
              "-DENABLE_BZIP2_PLUGIN=yes"
            ];
          };

          packageName = "simplon-stub-hs";
        in
        {
          packages.${packageName} =
            haskellPackages.callCabal2nix packageName self
              {
                inherit (pkgs) hdf5;
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
                pkg-config
                hdf5
              ];
              inputsFrom = [ self.packages.${system}.simplon-stub-hs.env ];
              HDF5_PLUGIN_PATH = "${hdf5-external-filter-plugins}/lib/plugins";
            };
          devShell = self.devShells.${system}.default;
        });
}
