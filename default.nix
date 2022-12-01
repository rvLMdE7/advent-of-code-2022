let
  pkgs = import <nixpkgs> {};
in
  pkgs.haskell.packages.ghc902.developPackage {
    root = ./.;
    name = "adventofcode2022";
    modifier = deriv: pkgs.haskell.lib.addBuildTools deriv [
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.stack
      pkgs.haskellPackages.haskell-language-server
    ];
  }
