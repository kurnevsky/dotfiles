# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='[\u@\h \W]\$ '

export EPM_INIT_FILE=~/.emacs.d/epm.el

export ALTERNATE_EDITOR=nano
export EDITOR=emacsclient

[ -f /usr/bin/src-hilite-lesspipe.sh ] && {
  export LESSOPEN="| /usr/bin/src-hilite-lesspipe.sh %s"
  export LESS=' -R '
}

export GPG_TTY=$(tty)

# It causes segfaults
export MAGICK_OCL_DEVICE=OFF

alias epm='~/.emacs.d/elpa/epm-*/epm'
alias ls='ls --color=auto'
alias grep='grep --color=auto'
