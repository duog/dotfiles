self: super: let src = super.fetchgit {
url = "https://github.com/domenkozar/hie-nix";
  # owner = "domenkozar";
  # repo = "hie-nix";
  rev = "96af698f0cfefdb4c3375fc199374856b88978dc";
  sha256 = "1ar0h12ysh9wnkgnvhz891lvis6x9s8w3shaakfdkamxvji868qa";
  };
in  with import src {pkgs = self;}; {
  inherit hies hie80 hie82 hie84;
  }
