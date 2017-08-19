{ pkgs ? import <nixpkgs> {} }:
let
  result = pkgs.haskell.packages.ghcjs.callCabal2nix "miso" (pkgs.fetchFromGitHub {
    sha256 = "0l6dscqhniq8my49ycgid3jwxkn7dgv680cjby9qn063kf2k64df";
    rev = "8475a6bbc0832e730f9556a371f8131a8b421230";
    owner = "haskell-miso";
    repo = "miso";
  }) {};
  hs2048 = pkgs.haskell.packages.ghcjs.callPackage ./hs-2048.nix {
    miso = result;
  };
  inherit (pkgs) sass closurecompiler;
in
  pkgs.runCommand "hs2048" { inherit hs2048; } ''
    mkdir -p $out
    ${sass}/bin/sass ${hs2048.src}/static/main.scss $out/main.css
    ${closurecompiler}/bin/closure-compiler ${hs2048}/bin/app.jsexe/all.js > $out/all.js
    cp ${hs2048.src}/static/index.html $out/index.html
  ''
