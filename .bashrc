# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='[\u@\h \W]\$ '

export ALTERNATE_EDITOR=nano
export EDITOR=emacsclient
export VIEWER=less
export AUR_PAGER=mc

if command -v src-hilite-lesspipe.sh > /dev/null
then
  export LESSOPEN="| src-hilite-lesspipe.sh %s"
  export LESS=' -R '
fi

export GPG_TTY=$(tty)

# It causes segfaults
export MAGICK_OCL_DEVICE=OFF

# Set cursor type to steady bar
echo -e -n "\x1b[\x36 q"
