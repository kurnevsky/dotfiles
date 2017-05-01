# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='[\u@\h \W]\$ '

export EDITOR=emacsnw

alias ls='ls --color=auto'
alias grep='grep --color=auto'
