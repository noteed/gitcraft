{ nixpkgs ? <nixpkgs>
}:

let
  pkgs = import nixpkgs {};
  design-system = pkgs.fetchFromGitHub {
    owner = "hypered";
    repo = "design-system";
    rev = "00ec56920f1c86baa5ea0fdf96e5be79ce253fc6";
    sha256 = "04i1hbfvsd068zgrlxgppia9ryxv2l33h00aaj9l0grkcldlkr8c";
  };
  inherit (import design-system {}) to-html;

  callPackage = pkgs.lib.callPackageWith pkgs.haskell.packages.ghc865;
  f = import ./derivation.nix;

in
rec {
  html.index = to-html ./index.md;

  html.all = pkgs.runCommand "all" {} ''
    mkdir $out
    cp ${html.index} $out/index.html

    mkdir -p docs/images
    cp ${./gitcraft.svg} gitcraft.svg
    ${gitcraft}/bin/gitcraft
    mv docs/images $out/
  '';

  gitcraft = callPackage f { stdenv = pkgs.stdenv; };
}
