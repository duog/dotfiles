let 
# pkgs = import ((import <nixpkgs> {}).fetchgit {
#   url = "https://github.com/NixOs/nixpkgs";
#   rev = "fab83fd9d591c1478a646a7915f0eeb3f75464d6";
#   sha256 = "0cslqlw0crfyy0mvgh414vbrq3xvxzb63w3afpj41d04zddb66qh";
# }) {};
pkgs = import <nixpkgs> {};
in { compiler ? "ghc843"
}: let
haskellPackages = (import ./. { inherit pkgs compiler; });
in haskellPackages.shellFor {
  packages = p: [ p.doug-xmonad ];
  withHoogle = true;
  buildInputs = [ pkgs.cabal-install pkgs.stack
    ];
}
