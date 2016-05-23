{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7103" }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithHoogle (ps: with ps; [
      # dev tools
      codex hscope hdevtools hasktags ghc-mod QuickCheck

      # imports
      mtl scientific
  ]);
in
pkgs.stdenv.mkDerivation rec {
  name = "haskell-play";
  buildInputs = [ ghc ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}

