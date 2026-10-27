let
  sources = import ./nix/26.05/nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  inherit (import sources.hypered-design) to-html;

  callPackage = pkgs.lib.callPackageWith pkgs.haskellPackages;
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

  gitcraft = callPackage f { lib = pkgs.lib; };
}
