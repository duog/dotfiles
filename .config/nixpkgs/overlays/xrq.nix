self: super: {
  xrq = with self.stdenv; mkDerivation {
    name = "xrq";
    version = "0.1";
    buildInputs = [ self.xorg.libX11 ];
    src = self.fetchFromGitHub {
      owner = "arianon";
      repo = "xrq";
      rev = "d5dc19c63881ebdd1287a02968e3a1447dde14a9";
      sha256 = "1bxf6h3fjw3kjraz7028m7p229l423y1ngy88lqvf0xl1g3dhp36";

    };

    outputs = [ "out" "man" ];

    installPhase = ''
      mkdir -p $out/bin
      mkdir -p $man
      cp xrq $out/bin
      cp xrq.1 $man/man
      '';

    # meta = {
    #   description = "A program for querying the X Resources Database from the command line.";
    #   homepage = "https://github.com/arianon/xrq";

    # };
  };
}
