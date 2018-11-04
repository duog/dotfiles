# if interactive
if [[ $- == *i* ]]; then
    # this sets up automatic theme changing and tmux
    if [[ $TERM == xterm* ]]; then
        function exit() {
            if [ -z "$TMUX" ]; then
                builtin exit
            else
                PANES=`tmux list-panes | wc -l`
                if [ $PANES -gt 1 ]; then
                    builtin exit
                else
                    tmux kill-window \; detach
                fi
            fi
        }
        BASE16_SHELL="$THEME_DIR/shell"
        BASE16_THEME_LINK="$THEME_DIR/base16_theme.sh"
        source "$BASE16_SHELL/realpath/realpath.sh"
        [ -f "$BASE16_THEME_LINK" ] && source "$BASE16_THEME_LINK"
        (inotifywait -m -q -e close_write "$THEME_DIR/current_theme" | (
                while true; do
                    read -t 1 TMP
                    [ -f "$BASE16_THEME_LINK" ] && source "$BASE16_THEME_LINK"
        done
            ) &
        )
        tmux has-session -t global-session > /dev/null
        if [ $? -ne 0 ]; then
            tmux new-session -d -s global-session
        fi
        CLIENTID=`shuf -n1 ~/.nix-profile/share/dict/words.txt | sed 's/\W//g'`
        #todo this makes an extra session
        exec tmux new-session -d -t global-session -s $CLIENTID \; set-option destroy-unattached \; new-window \; attach-session -t $CLIENTID
    fi

    if (command -v direnv &> /dev/null ); then
        if [[ -n "$ZSH_VERSION" ]]; then
            eval "$(direnv hook zsh)"
        elif [[ -n "$BASH_VERSION" ]]; then
            eval "$(direnv hook bash)"
        fi
    fi
fi

alias sysu='systemctl --user'
# TODO don't hardcode ~/code/home
alias nixup='nix-env -f "<nixpkgs>" -i --remove-all -A desktopEnv'
