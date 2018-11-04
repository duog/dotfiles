# let unstable = import <unstable> { config.allowUnfree = true; };
# in
self: super: with self; {
  desktopEnv = buildEnv {
  name = "desktopEnv";
  paths = [
    emacs irony-server ag ispell ripgrep
    # hies

    direnv cachix cacert yadm pass git mercurial zsh inotify-tools tmux
    scowl # for share/dicts/words
    nox fzf fasd wget vim unzip htop whois gnumake jq nix-prefetch-git gnupg
    openssl htop bmon

    haskellPackages.xmonad rofi rofi-pass dunst redshift polybar feh compton
    udiskie networkmanagerapplet nitrogen xclip xsel xdotool dzen2 xrq

    firefox alacritty xst steam electrum

    ];

    # LOCALE_ARCHIVE = "${glibcLocales}/locale/locale-archive";
    pathsToLink = [ "/" ];
    extraOutputsToInstall = [ "doc" "man" "info" ];
    postBuild = ''
    if [ -x $out/bin/install-info -a -w $out/share/info ]; then
      shopt -s nullglob
      for i in $out/share/info/*.info $out/share/info/*.info.gz; do
          $out/bin/install-info $i $out/share/info/dir
      done
    fi
    '';
  };
}
