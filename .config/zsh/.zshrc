
if [[ -s "${ZDOTDIR}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR}/.zprezto/init.zsh"
fi

[[ -e ~/.bashrc ]] && source ~/.bashrc

if [[ "$TERM" != dumb ]]; then
    ZSH_HIGHLIGHT_STYLES[cursor]=underline
fi

# DEFAULT_USER is not shown in prompt
DEFAULT_USER=doug

POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(context dir)
#TODO disable right prompt inside emacs

if [[ -n ${INSIDE_EMACS:-x} ]]; then
    POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=()
fi
