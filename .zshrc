[[ $TERM == 'dumb' ]] && unsetopt zle && PS1='$ ' && return

# History file
HISTFILE=~/.histfile
# The number of lines the shell will keep within one session
HISTSIZE=25000
# The number of lines of history will be saved
SAVEHIST=20000
PROMPT='[%n@%m %~]$ '
# Maximum input length for zsh-autosuggestions
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=10

# Remove all duplicates of current command from history, add current to end
setopt hist_ignore_all_dups
# Don't save any commands beginning with space
setopt hist_ignore_space
# Enable extended globs to interpret things like rm ^(file|file2)
setopt extended_glob
# Don't beep even if zsh don't like something
setopt no_beep
# Change directory even if user forgot to put 'cd' command in front, but entered path is valid
setopt auto_cd
# If possible, correct commands
setopt correct
# Append their history list to the history file, rather than replace it
setopt append_history
# If a pattern for filename generation has no matches, print an error, instead of leaving it unchanged in the argument list
setopt nomatch
# Report the status of background jobs immediately, rather than waiting until just before printing a prompt
setopt notify
# Allow parameter expansion, command substitution and arithmetic expansion for prompt string
setopt prompt_subst
# Remove any right prompt from display when accepting a command line
setopt transient_rprompt
# File completion after =
setopt magic_equal_subst
# Apply globbing to hidden files
setopt glob_dots

# Syntax highlighting
[ -f /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ] && \
  source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
# Automatically search the official repositories through pkgfile, when entering an unrecognized command
[ -f /usr/share/doc/pkgfile/command-not-found.zsh ] && \
  source /usr/share/doc/pkgfile/command-not-found.zsh
# Autocomplete with history.
[ -f /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh ] && \
  source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

# Set LS_COLORS environment variable
eval $(dircolors)

# Autocompletion with an arrow-key driven interface
zstyle ':completion:*' menu select
# Rehash automatically
zstyle ':completion:*:commands' rehash true
# Verbose completion results
zstyle ':completion:*' verbose true
# Enable corrections
zstyle ':completion:*' completer _complete _correct
# Case-insensitive completion, completion of dashed values
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[_-]=* r:|=*'
# Don't insert a literal tab when trying to complete in an empty buffer
zstyle ':completion:*' insert-tab false
# Group results by category
zstyle ':completion:*:matches' group true
zstyle ':completion:*' group-name ''
# Keep directories and files separated
zstyle ':completion:*' list-dirs-first true
# Use ls-colors for path completions
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
# Advanced process completion
zstyle ':completion:*:*:*:*:processes' command 'ps -A -o pid,user,%cpu,cmd'
zstyle ':completion:*:*:*:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;32'
# List all processes for killall
zstyle ':completion:*:processes-names' command "ps -eo cmd= | sed 's:\([^ ]*\).*:\1:;s:\(/[^ ]*/\)::;/^\[/d'"
# Display message when no matches are found
zstyle ':completion:*:warnings' format '%BSorry, no matches for: %d%b'
# Ignore internal zsh functions
zstyle ':completion:*:functions' ignored-patterns '_*'

# Sets autocompletion
autoload -Uz compinit && mkdir -p ~/.cache/zsh && compinit -d ~/.cache/zsh/zcompdump-$ZSH_VERSION
# Enable colors in prompt
autoload -Uz colors && colors
# Massive rename
autoload -Uz zmv
# Calculator
autoload -Uz zcalc
# Search history by entered text
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

# Autocompletion for kubernetes
command -v kubectl > /dev/null && source <(kubectl completion zsh)

# Many programs change the terminal state, and often do not restore terminal settings on exiting abnormally
# This avoids the need to manually reset the terminal
ttyctl -f

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

alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias zmv='noglob zmv -W'
aliases[=]='noglob zcalc -e'

export PATH=~/.bin-bw:~/.bin:~/.cabal/bin:$PATH

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

[[ -z "$MC_SID" ]] && command -v starship > /dev/null && eval "$(starship init zsh)"
