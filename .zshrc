[[ $TERM == 'dumb' ]] && unsetopt zle && PS1='$ ' && return

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
