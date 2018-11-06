{ nixpkgs ? import <nixpkgs> {}

 }:
let nixpkgs' = nixpkgs;
in with nixpkgs'.pkgs;
let hp = haskellPackages.override{
    overrides = self: super: {
      X11-rm = self.callPackage ./X11-rm.nix {};
      };};
     getHaskellDeps = ps: path:
        let f = import path;
            gatherDeps = { buildDepends ? [], libraryHaskellDepends ? [], executableHaskellDepends ? [], libraryToolDepends ? [], executableToolDepends ? [], ...}:
               buildDepends ++ libraryHaskellDepends ++ executableHaskellDepends ++ libraryToolDepends ++ executableToolDepends;
            x = f (builtins.intersectAttrs (builtins.functionArgs f) ps // {stdenv = stdenv; mkDerivation = gatherDeps;});
        in x;
ghc = hp.ghcWithPackages (ps: with ps; stdenv.lib.lists.subtractLists
[X11-rm]
([ cabal-install 

  ]  ++ getHaskellDeps ps ./X11-rm.nix));
in
pkgs.stdenv.mkDerivation {
  name = "my-haskell-env-0";
  buildInputs = [ ghc ];
  shellHook = ''
 export LANG=en_US.UTF-8
 eval $(egrep ^export ${ghc}/bin/ghc)
'';
}
