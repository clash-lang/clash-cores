{ nixpkgs ? import ./nix/nixpkgs.nix {} }:

with nixpkgs.pkgs;
with gitignore;

let
  clash-cores = haskellPackages.callCabal2nix "clash-cores" (gitignoreSource ./.) {};
in
  clash-cores.overrideAttrs (final: prev: {
    buildInputs = prev.buildInputs ++ [ pkgs.git ];
  })

