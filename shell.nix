{ nixpkgs ? import ./nix/nixpkgs.nix {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  project = import ./release.nix;
in

pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = project.env.nativeBuildInputs ++ [
    haskellPackages.cabal-install
    haskellPackages.haskell-language-server
  ];
  LC_ALL = "C.UTF-8";
}
