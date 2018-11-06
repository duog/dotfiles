# { pkgs ?  import <nixpkgs> { }
# , compiler ? "ghc843"
# , profiling ? false
# , tests ? false
# , haddock ? false
# , haskellPackages ?
# let hp = pkgs.haskell.packages.${compiler}.override {
#   overrides = self: super: {
#   xmonad = super.xmonad.overrideAttrs (oldAttrs:  { postInstall = ""; });
#       # xmonad = super.callPackage ./xmonad { haskellPackages = hp; };
#       # xmonad-contrb = super.callPackage ./xmonad-contrib { haskellPackagse = hp; };
#     };
#     packageSetConfig = self: super: {
#       mkDerivation = drv: let
#         newDrv = drv // {
#               enableLibraryProfiling = profiling;
#               enableExecutableProfiling = profiling;
#               doCheck = tests;
#               doHaddock = haddock;
#               configureFlags = (drv.configureFlags or []) ++ [
#                 "--ghc-option=-optl-fuse-ld=gold"
#                 "--ld-option=-fuse-ld=gold"
#                 "--with-ld=ld.gold"
#                 ];
#                 # setupCompileFlags = (drv.setupCompileFlags or []) ++ [
#                 # "--ghc-option=-optl-fuse-ld=gold"
#                 # "--ld-option=-fuse-ld=gold"
#                 # "--with-ld=ld.gold"
#                 # ];
#               };
#         in  (super.mkDerivation newDrv);
#     };
#   }; in hp
# }:

# # Strip out the irrelevant parts of the source
# let src = with pkgs.lib; with builtins; let
#       p = n: all (d: (toString d) != n) [ ./dist ./dist-newstyle ./.stack-work ];
#       in cleanSourceWith {filter = (n: t: p n); src = cleanSource ./.;};
#     extraEnvPackages = [
#     ];

#     packageName = baseNameOf ./.;
#     drv = haskellPackages.callCabal2nix packageName src {};

#     envWithExtras = pkgs.lib.overrideDerivation drv.env (attrs: {
#       buildInputs = attrs.buildInputs ++ extraEnvPackages;
#     });
# in
# drv // { env = envWithExtras; }

{ pkgs ? import <nixpkgs> {}
, compiler ? "ghc843"
}: let
haskellPackages = pkgs.haskell.packages."${compiler}";
in haskellPackages.extend (haskellPackages.packageSourceOverrides {
  doug-xmonad = ./.;
})
