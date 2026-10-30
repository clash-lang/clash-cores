{
  description = "A flake for the clash-cores";
  inputs = {
    clash-compiler.url = "github:clash-lang/clash-compiler";
    clash-protocols-src = {
      url = "github:clash-lang/clash-protocols";
      flake = false;
    };
  };
  outputs = { self, flake-utils, clash-compiler, clash-protocols-src, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        # The 'default' version of ghc to use
        default-version = clash-compiler.ghcVersion.${system};
        # A list of all ghc versions this package supports
        supported-versions = clash-compiler.supportedGhcVersions.${system};

        regular-pkgs = import clash-compiler.inputs.nixpkgs {
          inherit system;
        };

        # Nixpkgs with clash-compiler's overlay for the given GHC version applied
        clash-pkgs-for = compiler-version:
          regular-pkgs.extend clash-compiler.overlays.${compiler-version};

        all-overlays = builtins.listToAttrs (builtins.map (compiler-version:
          let
            pkgs = clash-pkgs-for compiler-version;

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

            # clash-protocols ships each sub-package's CHANGELOG.md as a symlink
            # to the repo-root CHANGELOG.md (../CHANGELOG.md). Nix copies the
            # package directory without that out-of-root target, so the symlink
            # dangles in the build sandbox. Cabal >=3.12 (ghc9101+) merely warns
            # about the unmatched 'extra-doc-files' wildcard, but the Cabal 3.10
            # shipped with ghc96*/ghc98* treats it as a fatal error. Replace the
            # dangling symlink with the real file for those.
            protocols-changelog = "${clash-protocols-src}/CHANGELOG.md";
            # True for ghc96* / ghc98*, which ship Cabal 3.10.
            uses-cabal-3-10 =
              clash-compiler.inputs.nixpkgs.lib.hasPrefix "ghc96" compiler-version
              || clash-compiler.inputs.nixpkgs.lib.hasPrefix "ghc98" compiler-version;
            fixup-changelog = drv:
              if uses-cabal-3-10 then
                drv.overrideAttrs (pAttr: {
                  postPatch = pAttr.postPatch or "" + ''
                    rm -f CHANGELOG.md
                    cp ${protocols-changelog} CHANGELOG.md
                  '';
                })
              else
                drv;

            overlay = final: prev: {
              circuit-notation = final.callHackageDirect {
                pkg = "circuit-notation";
                ver = "0.2.0.0";
                sha256 = "sha256-tdM3spbXjQvcnBrmVS0i0tLqoHJ/pnniSOy3eTEZKuw=";
              } {};
              clash-protocols-base = fixup-changelog (prev.developPackage {
                root = "${clash-protocols-src}/clash-protocols-base";
                overrides = _: _: final;
              });
              clash-protocols = fixup-changelog ((prev.developPackage {
                root = "${clash-protocols-src}/clash-protocols";
                overrides = _: _: final;
                # See https://github.com/clash-lang/clash-protocols/issues/131
                modifier = drv: drv.overrideAttrs (_: { doCheck = false; });
              }));

              # Append the package set with clash-cores
              clash-cores = ((prev.developPackage {
                root = ./.;
                overrides = _: _: final;
              }).overrideAttrs override-attrs).overrideAttrs (pAttr: {
                preCheck = pAttr.preCheck or "" + ''
                  export TASTY_NUM_THREADS=$NIX_BUILD_CORES
                  export TASTY_HIDE_SUCCESSES=True
                '';
              });

              # The HDL testsuite, wrapped such that it uses the Clash and HDL
              # tools from Nix. Run it from the root of the repository:
              #
              #   nix run .#ghc9124-hdl-tests -- --hide-successes
              #
              # Based on clash-testsuite in clash-compiler's nix/overlay.nix.
              cores-hdl-tests =
                let
                  unmodified = prev.callCabal2nix "cores-hdl-tests" ./hdl-tests { };
                in
                unmodified.overrideAttrs (old: {
                  buildInputs = (old.buildInputs or [ ]) ++ [
                    pkgs.makeWrapper
                  ];

                  postInstall = (old.postInstall or "") + ''
                    ghcBinDir=${dirOf "${old.passthru.env.NIX_GHC}"}
                    ghcLibDir="${old.passthru.env.NIX_GHC_LIBDIR}"
                    globalPackageDb="$ghcLibDir/package.conf.d"
                    wrapperDir="$out/libexec/cores-hdl-tests-wrappers"

                    mkdir -p "$wrapperDir"

                    cat > "$wrapperDir/clash" <<EOF
                    #!${pkgs.runtimeShell}
                    exec ${final.clash-ghc}/bin/clash \
                      -package-db "$globalPackageDb" \
                      "\$@"
                    EOF
                    chmod +x "$wrapperDir/clash"

                    wrapProgram $out/bin/cores-hdl-tests \
                      --prefix PATH : "$ghcBinDir" \
                      --prefix PATH : "$wrapperDir" \
                      --set GHC_PACKAGE_PATH "$globalPackageDb:" \
                      --set USE_GLOBAL_CLASH 1 \
                      --prefix PATH : ${pkgs.lib.makeBinPath [
                        pkgs.gcc
                        # make and python3 are used in verilator invocations
                        pkgs.gnumake
                        pkgs.python3
                        pkgs.ghdl-llvm
                        pkgs.sby
                        pkgs.verilator
                        pkgs.iverilog
                        pkgs.yosys
                        pkgs.z3
                      ]} \
                      --set LIBRARY_PATH ${pkgs.lib.makeLibraryPath [
                        pkgs.ghdl-llvm
                        pkgs.zlib.static
                      ]}
                  '';
                });
            };
          in
            { name = compiler-version; value = overlay; }
          ) supported-versions);

        all-hs-pkgs = builtins.mapAttrs (compiler-version: overlay:
          let
            pkgs = clash-pkgs-for compiler-version;
            clash-pkgs = pkgs."clashPackages-${compiler-version}";

            hs-pkgs = clash-pkgs.extend overlay;
          in
            hs-pkgs
          ) all-overlays;

        minimal-shell = hs-pkgs: hs-pkgs.shellFor {
          packages = p: [
            p.clash-cores
          ];

          # https://discourse.nixos.org/t/non-interactive-bash-errors-from-flake-nix-mkshell/33310
          buildInputs = [
            regular-pkgs.bashInteractive
          ];

          nativeBuildInputs = [
            hs-pkgs.cabal-install
            hs-pkgs.cabal-plan
            hs-pkgs.fourmolu
          ];
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
          }) all-hs-pkgs;

        all-packages = builtins.mapAttrs (_: hs-pkgs: hs-pkgs.clash-cores) all-hs-pkgs;

        # The HDL testsuite for each version, e.g. 'ghc9124-hdl-tests'
        all-hdl-tests = clash-compiler.inputs.nixpkgs.lib.attrsets.mapAttrs'
          (name: hs-pkgs: { name = "${name}-hdl-tests"; value = hs-pkgs.cores-hdl-tests; })
          all-hs-pkgs;
      in
      {
        # Expose the overlay of each supported version which adds clash-cores
        # The base of the overlay is clash-pkgs
        overlays = all-overlays // { default = all-overlays.${default-version}; };

        # A devShell for each supported version
        #
        # These can be invoked using `nix develop .#ghc9124-minimal`
        #
        # Please do note that if you work with Nix, you need to remove ALL the `cabal*.project` files at
        # the root of the directory! Cabal prioritizes local source overrides over Nix, which causes
        # the many packages to incorrectly be fetched.
        devShells = all-shells // { default = all-shells."${default-version}-minimal"; };

        # Packages for each version of GHC, with a default package being set to the default-version's version
        packages = all-packages // all-hdl-tests // { default = all-packages.${default-version}; };
      });
}
