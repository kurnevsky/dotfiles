# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='[\u@\h \W]\$ '

export EPM_INIT_FILE=~/.emacs.d/epm.el

export ALTERNATE_EDITOR=nano
export EDITOR=emacsclient

export GPG_TTY=$(tty)

alias epm='~/.emacs.d/elpa/epm-*/epm'
alias ls='ls --color=auto'
alias grep='grep --color=auto'
