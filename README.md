# Clash Cores
**Warning: Still under construction**

This repository contains a number of existing useful circuits written in Clash.

These circuits include:
- SPI core
- UART core
- CRC core
- Etherbone core
- 8b10b line encoder/decoder
- SGMII PCS receiver/transmitter
- Various wrappers around xilinx IPs (blockram, dcfifo, floating, ila, xpm ...)

Note: The `SGMII` circuits require `clash-prelude >= 1.9.0` and will be skipped if built against an earlier version of Clash.

This code was originally a sub-folder within the clash-compiler repo, but now it's being moved into its own repository. This move is still in progress. You can see the current status of move [here](https://github.com/clash-lang/clash-compiler/issues/2757).

If you're looking for a pre-built Clash circuit and don't find it here, you can also check out [Clash Protocols](https://github.com/clash-lang/clash-protocols/tree/main/clash-protocols/src/Protocols) to see if it exists there.

You can also look through Adam Walker's excellent list of [prebuilt Clash circuits](https://github.com/adamwalker/clash-utils) and other community projects on Clash's [ecosystem page](https://clash-lang.org/ecosystem/).

# Nix
This project exposes several Nix flake outputs. Most notably the `clash-cores` package, exposed individually or as an overlay (which you need to apply on top of clash-compiler).

## Usage

Contributing to `clash-cores` can be done via the developer shell exposed by the Nix flake. Open a terminal and type: `nix develop`. Optionally a specific GHC version can be selected as well as a `-minimal` or `-full` version. The `-minimal` shell does **NOT** contain the Haskell Language Server, whilst the `-full` shell does. When using the developer shell, do not forget to remove the `cabal.project` file. This file contains a package source and Cabal prioritizes local package sources over Nix sources. Removing the `cabal.project` file should work fine when developing with Nix.

Compiling Clash and all dependencies related to it may take a long time. To remidy long compilation times, you can make use of the publically available cache on Cachix:

### Cachix (binary cache)

Cachix contains all commits on the main branch of `clash-cores` since the cache has been setup. It is recommended to add the publically available cachix cache to your project/computer to minimize the amount of time manually compiling Clash related dependencies.

You can either add user-wide on your local system via `nix.conf` (usually located under `~/.config/nix`, create it if it does not exist):
```conf
extra-substituters = https://clash-lang.cachix.org
extra-trusted-public-keys = clash-lang.cachix.org-1:/2N1uka38B/heaOAC+Ztd/EWLmF0RLfizWgC5tamCBg=
```

Or as part of your `flake.nix` in your local project:
```nix
{
  nixConfig = {
    extra-substituters = [ "https://clash-lang.cachix.org" ];
    extra-trusted-public-keys = [ "clash-lang.cachix.org-1:/2N1uka38B/heaOAC+Ztd/EWLmF0RLfizWgC5tamCBg=" ];
  };
  description = ...;
  inputs = ...;
  outputs = ...;
}
```

## As a dependency

You can also add this project as a Nix-dependency. This project depends on `clash-compiler`, the recommended way to add this to your project as a Nix dependency is as follows:

First add `clash-compiler` and `clash-cores` to your flake inputs:
```nix
inputs = {
  clash-compiler.url = "github:clash-lang/clash-compiler"
  clash-cores = {
    url = "github:clash-lang/clash-cores"
    inputs.clash-compiler.follows = "clash-compiler";
  };
};
```

It is important to have clash-cores follow the same clash-compiler version as your project, this ensures you do not get different Haskell package hashes. Afterwards, use the `clashPackages` as a baseline for your Haskell packages and overlay this project's overlay on top of it:

```nix
let
  # The GHC version you would like to use
  # This has to be be one of the supported versions of clash-compiler
  compiler-version = "ghc9101";

  # Import the normal and Haskell package set from clash-compiler
  pkgs = (import clash-compiler.inputs.nixpkgs {
    inherit system;
  }).extend clash-compiler.overlays.${compiler-version};
  clash-pkgs = pkgs."clashPackages-${compiler-version}";

  # Define your Haskell package
  overlay = final: prev: {
    # Your haskell package here
    my-package = prev.developPackage {
      root = ./my-source;
      overrides = _: _: final;
    };
  }
  # Make sure to include the clash-cores overlay!
  # This is what brings the clash-cores packages into scope
  // clash-cores.overlays.${system}.default final prev;

  # The final Haskell package set, containing your project as well as all Clash dependencies
  hs-pkgs = clash-pkgs.extend overlay;
in
  {
    # .. your outputs
  }
```

And that's it!

