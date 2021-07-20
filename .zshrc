[[ $TERM == 'dumb' ]] && unsetopt zle && PS1='$ ' && return

# Find the key with: showkey -a
bindkey -e
[[ -n "${terminfo[khome]}" ]] && bindkey "${terminfo[khome]}" beginning-of-line
[[ -n "${terminfo[kend]}" ]] && bindkey "${terminfo[kend]}" end-of-line
bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word
bindkey "\e[3~" delete-char
bindkey '^ ' autosuggest-accept # Ctrl+Space
# Terminal can send different control sequences depending on whether it has been put in keypad transmit mode or not.
# The smkx and rmkx terminfo entries can be used to put a terminal in or out of that mode.
[[ -n "${terminfo[kcuu1]}" ]] && bindkey "${terminfo[kcuu1]}" up-line-or-beginning-search # Up
[[ -n "${terminfo[kcuu1]/O/[}" ]] && bindkey "${terminfo[kcuu1]/O/[}" up-line-or-beginning-search # Up
[[ -n "${terminfo[kcud1]}" ]] && bindkey "${terminfo[kcud1]}" down-line-or-beginning-search # Down
[[ -n "${terminfo[kcud1]/O/[}" ]] && bindkey "${terminfo[kcud1]/O/[}" down-line-or-beginning-search # Down

if command -v sk > /dev/null
then
  skim-history-widget() {
    local num
    setopt localoptions pipefail
    echo -ne "\r"
    num=$(fc -rl 1 | sk --height 50% -n2..,.. --tiebreak=score,index --layout=reverse --inline-info -p "➜ " --query="$LBUFFER")
    local ret=$?
    zle reset-prompt
    if [ -n "$num" ]; then
      zle vi-fetch-history -n "$num"
    fi
    return $ret
  }
  zle -N skim-history-widget
  bindkey '^R' skim-history-widget
fi

alias zmv='noglob zmv -W'
aliases[=]='noglob zcalc -e'

export PATH=~/.bin:$PATH

export ALTERNATE_EDITOR=nano
export VIEWER=less

if command -v src-hilite-lesspipe.sh > /dev/null
then
  export LESSOPEN="| src-hilite-lesspipe.sh %s"
  export LESS=' -R '
fi

# It causes segfaults
export MAGICK_OCL_DEVICE=OFF

# Set cursor type to steady bar
echo -e -n "\x1b[\x36 q"

[[ -z "$MC_SID" ]] && command -v starship > /dev/null && eval "$(starship init zsh)"
