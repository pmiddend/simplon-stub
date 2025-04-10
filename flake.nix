{
  description = "simplon-stub-hs";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-24.11";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      top-level-pkgs = import nixpkgs {
        inherit system;
        overlays = [ ];
      };

      haskellPackages = top-level-pkgs.haskellPackages.override
        {
          # This is a hack. We don't need hdf5 from hackage at all, but "pkgconfig-depends" in the cabal file
          # forces the usage of hdf5 somehow. So we map it to an existing, used package and everything works.
          overrides = self: super: {
            # hdf5 = top-level-pkgs.haskell.lib.markUnbroken super.hdf5;
            # hdf5 = top-level-pkgs.stdenv.mkDerivation { name = "myhdf5"; };
            # Looking weird? Rightfully so.
            # hdf5 = super.text;
          };
          # overrides = self: super: { };
        };

      hdf5-external-filter-plugins = with top-level-pkgs; stdenv.mkDerivation rec {
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
        buildInputs = [ hdf5-threadsafe lz4 bzip2 ];

        cmakeFlags = [
          "-DENABLE_BITSHUFFLE_PLUGIN=yes"
          "-DENABLE_LZ4_PLUGIN=yes"
          "-DENABLE_BZIP2_PLUGIN=yes"
        ];
      };

      packageName = "simplon-stub-hs";
    in
    {
      packages.${system}.default =
        haskellPackages.callCabal2nix packageName self
          {
            # inherit (top-level-pkgs) hdf5;
            hdf5 = top-level-pkgs.hdf5-threadsafe;
            scotty = haskellPackages.scotty_0_22;
          };

      devShells.${system}.default =
        top-level-pkgs.mkShell {
          buildInputs = with top-level-pkgs;
            [
              haskellPackages.haskell-language-server # you must build it with your ghc to work
              cabal-install
              ghcid
              haskellPackages.hlint
              haskellPackages.apply-refact
              pkg-config
              hdf5-threadsafe
            ];
          inputsFrom = [ self.packages.${system}.default.env ];
          HDF5_PLUGIN_PATH = "${hdf5-external-filter-plugins}/lib/plugins";
        };

      nixosModules.simplon-stub = { pkgs, config, lib, ... }:
        {
          options.services.simplon-stub = with pkgs.lib; {
            enable = lib.mkEnableOption "enable simplon stub";

            http-port = mkOption { type = types.port; default = 10001; };
            input-h5-file = mkOption { type = types.path; };
          };

          config = lib.mkIf config.services.simplon-stub.enable {
            users.groups.simplon = { };

            users.users.simplon = {
              createHome = true;
              isSystemUser = true;
              home = "/var/lib/simplon";
              group = "simplon";
            };

            systemd.services.simplon-stub =
              {
                description = "Simplon HTTP stub server";
                after = [ "network.target" ];
                wantedBy = [ "multi-user.target" ];
                environment.HDF5_PLUGIN_PATH = "${hdf5-external-filter-plugins}/lib/plugins";

                serviceConfig = {
                  User = "simplon";
                  Group = "simplon";
                  ExecStart = "${self.packages.${system}.default}/bin/simplon-stub --input-h5-file ${config.services.simplon-stub.input-h5-file} --listen ${toString config.services.simplon-stub.http-port} --zmq-bind-address 'tcp://*:9999'";
                };
              };

          };
        };

      checks.${system}.vmTest = top-level-pkgs.nixosTest {
        name = "test-simple-http";

        nodes = {
          server = { pkgs, ... }: {
            imports = [ self.nixosModules.simplon-stub ];

            services.simplon-stub = {
              enable = true;
              input-h5-file = pkgs.writeText "input.h5" "test";
            };
          };
        };

        testScript =
          ''
            start_all()
            server.wait_for_unit("simplon-stub", timeout=10)
            server.wait_for_open_port(9999, timeout=10)
            server.wait_until_succeeds("${top-level-pkgs.curl}/bin/curl --fail http://localhost:10001/stream/api/1.8.0/config/mode", timeout=10)
          '';
      };
    };

}
