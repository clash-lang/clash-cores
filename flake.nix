{ 
  description = "A flake for the clash-cores";
  inputs = {
    clash-compiler.url = "github:clash-lang/clash-compiler";
    clash-protocols = {
      url = "github:clash-lang/clash-protocols";
      inputs.clash-compiler.follows = "clash-compiler";
    };
  };
  outputs = { self, flake-utils, clash-compiler, clash-protocols, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        # The 'default' version of ghc to use
        default-version = clash-compiler.ghcVersion.${system};
        # A list of all ghc versions this package supports
        supported-versions = clash-compiler.supportedGhcVersions.${system};

        all-overlays = builtins.listToAttrs (builtins.map (compiler-version:
          let
            # Remove the -fplugin and Setup.hs settings in the .cabal
            # For ghc9101+ these options don't matter, but for ghc964 and ghc982 this breaks installation
            # When entering the installPhase something (I'm not entirely sure what) goes wrong
            # between Nix and GHC, causing Setup.hs to get invoked with the wrong set of packages
            # (I think?). Removing the specific flags during installation fixes the issue for Nix,
            # whilst not breaking regular compilation.
            #
            # Do note that this patch only gets applied during *installation* and not *compilation*
            # That means these flags are still in place during compilation
            override-attrs = if compiler-version == "ghc964" || compiler-version == "ghc982" then
                fAttr: pAttr: {
                  preInstall = pAttr.preInstall or "" + ''
                    sed -i "/-fplugin GHC.TypeLits.Extra.Solver/,+2d" clash-cores.cabal
                  '';
                }
              else
                {};

            overlay = final: prev: {
              # Append the package set with clash-cores
              clash-cores = (prev.developPackage {
                root = ./.;
                overrides = _: _: final;
              }).overrideAttrs override-attrs;
            } // clash-protocols.overlays.${system}.${compiler-version} final prev;

            hdl-tests-overlay = final: prev: {
              # The testsuite
              cores-hdl-tests = (prev.developPackage {
                root = ./hdl-tests;
                overrides = _: _: final;
              });
            };
          in
            { name = compiler-version; value = { base = overlay; testsuite = hdl-tests-overlay; }; }
          ) supported-versions);

        all-hs-pkgs = builtins.mapAttrs (compiler-version: overlay:
          let
            pkgs = (import clash-compiler.inputs.nixpkgs {
              inherit system;
            }).extend clash-compiler.overlays.${compiler-version};
            clash-pkgs = pkgs."clashPackages-${compiler-version}";

            hs-pkgs = (clash-pkgs.extend overlay.base).extend overlay.testsuite;
          in
            hs-pkgs
          ) all-overlays;

        minimal-shell = hs-pkgs: hs-pkgs.shellFor {
          packages = p: [
            p.clash-cores
            p.cores-hdl-tests
          ];

          nativeBuildInputs =
            [
              hs-pkgs.cabal-install
              hs-pkgs.cabal-plan
              hs-pkgs.fourmolu
            ]
          ;
        };

        all-shells = clash-compiler.inputs.nixpkgs.lib.attrsets.concatMapAttrs (name: hs-pkgs: {
            # The difference between the `-minimal` and `-full` is the addition of HLS in the full version
            # This is because HLS is slow to compile and not everyone uses it
            # We default to using the `-minimal` version when `nix develop`ing
            "${name}-minimal" = minimal-shell hs-pkgs;
            "${name}-full" = (minimal-shell hs-pkgs).overrideAttrs (fAttr: pAttr: {
              nativeBuildInputs = pAttr.nativeBuildInputs ++ [
                hs-pkgs.haskell-language-server
              ];
            });
          }) all-hs-pkgs // (
          let
            hs-pkgs = all-hs-pkgs.${default-version};
          in {
            # This devshell is only meant to be used for the developer shell
            # It includes the cores-hdl-tests package
            # CI only uses default version for HDL tests
            ci = hs-pkgs.shellFor {
              packages = p: [
                hs-pkgs.cores-hdl-tests
              ];

              nativeBuildInputs =
                [
                  hs-pkgs.cabal-install
                  hs-pkgs.cabal-plan
                  hs-pkgs.fourmolu

                  hs-pkgs.cores-hdl-tests
                ];
            };
          });

        all-packages = builtins.mapAttrs (_: hs-pkgs: {
          cores-hdl-tests = hs-pkgs.cores-hdl-tests;
          clash-cores = hs-pkgs.clash-cores;
          default = hs-pkgs.clash-cores;
        }) all-hs-pkgs;
      in
      {
        # Expose the overlay of each supported version which adds clash-cores
        # The base of the overlay is clash-pkgs
        overlays = all-overlays // { default = all-overlays.${default-version}; };

        # A devShell for each supported version
        #
        # These can be invoked using `nix develop .#ghc9101-minimal`
        #
        # Please do note that if you work with Nix, you need to remove ALL the `cabal*.project` files at
        # the root of the directory! Cabal prioritizes local source overrides over Nix, which causes
        # the many packages to incorrectly be fetched.
        devShells = all-shells // { default = all-shells."${default-version}-minimal"; };

        # Packages for each version of GHC, with a default package being set to the default-version's version
        packages = all-packages // { default = all-packages.${default-version}.default; };
      });
}
