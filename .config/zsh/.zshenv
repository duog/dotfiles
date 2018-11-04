# Prevent /etc/profile being sourced
export __ETC_PROFILE_SOURCED=1

ZDOTDIR=${ZDOTDIR:-$HOME}

if [[ ( "$SHLVL" -eq 1 && ! -o LOGIN ) ]]; then
  [[ -f "/etc/zprofile" ]] && source /etc/zprofile
  [[ -f "$ZDOTDIR/.zprofile" ]] && source "$ZDOTDIR/.zprofile"
fi
