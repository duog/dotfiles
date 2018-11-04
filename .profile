
IS_NIXOS=${NIX_PATH+y}

# I believe NIXOS does all this in /etc/profile, so no need to do it again
if [[ -z ${IS_NIXOS} ]]; then
  [[ -f ~/.nix-profile/etc/profile.d/nix.sh ]] && source ~/.nix-profile/etc/profile.d/nix.sh
  export INFOPATH="$HOME/.nix-profile/info:$HOME/.nix-profile/share/info:$INFOPATH"
fi


# nix has already put ~/.nix-profile/bin etc in path
# emacs.d/bin was for doom
PATH=~/bin:~/.local/bin:~/.emacs.d/bin:$PATH

if [[ "$OSTYPE" == darwin* ]]; then
  export BROWSER='open'
fi


if [[ -z "$LANG" ]]; then
  export LANG='en_NZ.UTF-8'
fi

export EDITOR='ec'
# export VISUAL='nano'
export PAGER='less'
export TERMINAL='st'


# Set the default Less options.
# Mouse-wheel scrolling has been disabled by -X (disable screen clearing).
# Remove -X and -F (exit if the content fits on one screen) to enable it.
export LESS='-F -g -i -M -R -S -w -X -z-4'

# TODO these hardcode XDG_*. I need to, because I can't easily have the paths depend on environment variables when all the dotfiles are stored in yadm

export HOME_SRC=~/.config/my-home
export NIX_PATH=nixpkgs=$HOME/.nix-defexpr/channels/unstable:$NIX_PATH

export XMONAD_CONFIG_DIR=~/.config/xmonad
export XMONAD_CACHE_DIR=~/.cache/xmonad
export XMONAD_DATA_DIR=~/.local/share/xmonad


export SPACEMACSDIR=~/.config/spacemacs

export THEME_DIR="$HOME_SRC/theme"
