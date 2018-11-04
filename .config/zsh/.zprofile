[[ -f $HOME/.profile ]] && source $HOME/.profile

typeset -gU cdpath fpath mailpath path NIX_PATH

# Set the Less input preprocessor.
# Try both `lesspipe` and `lesspipe.sh` as either might exist on a system.
if (( $#commands[(i)lesspipe(|.sh)] )); then
  export LESSOPEN="| /usr/bin/env $commands[(i)lesspipe(|.sh)] %s 2>&-"
fi

if [[ "$TERM" = eterm-color ]]; then
    export TERM=emacs-256color;
fi
