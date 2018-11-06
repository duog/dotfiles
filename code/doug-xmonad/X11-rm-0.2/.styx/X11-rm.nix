{ mkDerivation, base, stdenv, X11 }:
mkDerivation {
  pname = "X11-rm";
  version = "0.2";
  src = /home/doug/code/X11-rm-0.2;
  libraryHaskellDepends = [ base X11 ];
  description = "A binding to the resource management functions missing from X11";
  license = stdenv.lib.licenses.bsd3;
}
