{
  description = "A flake for developing & using clash-cores";
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
        # What version of the GHC compiler to use for protocols
        compiler-version = clash-compiler.ghcVersion.${system};

        pkgs = (import clash-compiler.inputs.nixpkgs {
          inherit system;
        }).extend clash-compiler.overlays.${compiler-version};
        clash-pkgs = pkgs."clashPackages-${compiler-version}";

        overlay = final: prev: {
          clash-cores = prev.developPackage {
            root = ./.;
            overrides = _: _: final;
          };
        }
        # Make sure clash-protocols is in scope as well
        // (clash-protocols.overlays.${system}.default final prev);
        hs-pkgs = clash-pkgs.extend overlay;
      in
      {
        # Expose the overlay which adds clash-cores
        # (...along the inherited overlays)
        # The base of the overlay is clash-pkgs
        overlays.default = overlay;

        devShells = rec {
          minimal = hs-pkgs.shellFor {
            packages = p: [
              p.clash-cores
            ];

            nativeBuildInputs = [
              hs-pkgs.cabal-install
            ];
          };

          full = hs-pkgs.shellFor {
            packages = p: [
              p.clash-cores
            ];

            nativeBuildInputs =
              [
                hs-pkgs.cabal-install
                hs-pkgs.haskell-language-server
                hs-pkgs.fourmolu
              ]
            ;
          };

          default = minimal;
        };

        packages = {
          clash-cores = hs-pkgs.clash-cores;

          default = hs-pkgs.clash-cores;
        };
      });
}
