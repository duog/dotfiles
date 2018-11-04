#!/run/current-system/sw/bin/zsh

# We hardcode the path to zsh in nixos here.
# All our environment should have been sourced, due to our .zshenv

# these have control characters in them, they make systemd barf
unset $(env | grep LESS_TERMCAP |awk -F'=' '{print $1}')

# I think systemd doesn't like the newlines in this one?
# TODO Investiage
unset FZF_DEFAULT_OPTS

while [[ ! -f ~/.this_tty ]]; do
    sleep .1
done

export GPG_TTY=$(cat ~/.this_tty)
rm ~/.this_tty

systemctl --user import-environment
