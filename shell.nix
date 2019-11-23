let
  nixpkgs = builtins.fetchTarball {
    sha256 = "1fxmy6fmsx6fp3h2k9jyg4q41dfl1777wswk9bx3v9hzm114pjy7";
    url = https://github.com/nixos/nixpkgs/archive/e9d4bab04458cd03cb9029d2d571c37cc017ffdc.tar.gz;
  };
  reesd-stack = builtins.fetchTarball {
    url = https://github.com/noteed/nixpkgs-stackage/tarball/7992ea5e52ffc0d6cee607aa06f9b85822d9c418/reesd-stack.tar.gz;
    sha256 = "0h32bx7bdvij3klfl356f7r4yslmsv34b3bn1h8qq4dlfvzg0i7d";
  };
  pkgs = import nixpkgs {
    # Provide lts-921-reesd.
    overlays = [ (import reesd-stack) ] ;
  };
  hspkgs = pkgs.haskell.packages.stackage.lts-921-reesd;

  f = import ./derivation.nix;
  drv = hspkgs.callPackage f {};
in

  pkgs.lib.overrideDerivation drv.env (old: {
    name = "gitcraft-env";
    buildInputs = old.buildInputs ++ [ pkgs.cabal-install ];
    shellHook = ''
    '';
  })

