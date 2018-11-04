
#!/usr/bin/env bash

# from https://stackoverflow.com/questions/192249/how-do-i-parse-command-line-arguments-in-bash/33826763#33826763


cd "$THEME_DIR"

sourcedir="$(pwd)/result"

if [[ ! -f "$sourcedir/schemes" ]]; then
    echo "file $sourcedir/shcemes doesn't exist"; exit
fi

theme=${1:-$(cat "$sourcedir/schemes" | rofi -dmenu)}

if [[ -z "${theme}" ]]; then
    echo "No theme chosen"; exit
fi

# this works -- but the themes aren't great
# ln -fs "$sourcedir/emacs/build/${theme}-theme.el"
# emacsclient -e <<EOF > /dev/null
# (load-theme '${theme} t)
# EOF

(
    script="$sourcedir/shell/scripts/${theme}.sh"
    ln -fs $script "base16_theme.sh"
    echo -e "if \0041exists('g:colors_name') || g:colors_name != 'base16-$theme'\n  colorscheme base16-$theme\nendif" >| ".vimrc_background"
)

# (
    # base16-shell does this

    # cd ~/.config/alacritty
    # cat alacritty.template.yml "$sourcedir/alacritty/colors/${theme}.yml" > alacritty.yml
# )

(
    cd ~/.config/rofi
    ln -sf "$sourcedir/rofi/themes/$theme.rasi" my-theme.rasi
)

(
    cd ~/.config/Xresources
    ln -sf "$
sourcedir/xresources/xresources/${theme}.Xresources" Xresources.colours
    xrdb -merge ~/.Xresources
)

# TODO dunst

systemctl --user restart polybar

# must be last. An inotify process -- started in .bashrc -- is watching this file
echo "$theme" > current_theme
