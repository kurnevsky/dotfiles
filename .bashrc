# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='[\u@\h \W]\$ '

export ALTERNATE_EDITOR=nano
export EDITOR=emacsclient

export GPG_TTY=$(tty)

alias ls='ls --color=auto'
alias grep='grep --color=auto'
