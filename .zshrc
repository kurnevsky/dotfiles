[[ $TERM == 'dumb' ]] && unsetopt zle && PS1='$ ' && return

git_prompt() {
  if [ -z "$MC_SID" ] && git rev-parse --git-dir > /dev/null 2> /dev/null
  then
    local GIT_WHERE GIT_DETACHED GIT_AHEAD GIT_BEHIND GIT_STAGED \
          GIT_CHANGED GIT_UNMERGED GIT_UNTRACKED GIT_CLEAN GIT_STATUS
    GIT_WHERE=$(git symbolic-ref -q HEAD 2> /dev/null)
    if [ "$?" -eq 0 ]
    then
      GIT_DETACHED=1
    else
      GIT_DETACHED=0
      GIT_WHERE=$(git name-rev --name-only --no-undefined --always HEAD 2> /dev/null)
    fi
    GIT_WHERE=${GIT_WHERE#(refs/heads/|tags/)}
    GIT_AHEAD=$(git log --oneline @{u}.. 2> /dev/null | wc -l)
    GIT_BEHIND=$(git log --oneline ..@{u} 2> /dev/null | wc -l)
    GIT_STAGED=$(git diff --cached --diff-filter=u --name-only 2> /dev/null | wc -l)
    GIT_CHANGED=$(git diff --diff-filter=u --name-only 2> /dev/null | wc -l)
    GIT_UNMERGED=$(git diff --diff-filter=U --name-only 2> /dev/null | wc -l)
    GIT_CHANGED=$(( GIT_CHANGED - GIT_UNMERGED ))
    GIT_UNTRACKED=$(git ls-files --other --exclude-standard 2> /dev/null | wc -l)
    if [[ "$GIT_STAGED" -eq 0 && "$GIT_CHANGED" -eq 0 && "$GIT_UNMERGED" -eq 0 && "$GIT_UNTRACKED" -eq 0 ]]
    then
      GIT_CLEAN=0
    else
      GIT_CLEAN=1
    fi

    if [ "$GIT_DETACHED" -eq 0 ]
    then
      GIT_STATUS="(%{$fg_bold[yellow]%}$GIT_WHERE%{$reset_color%}"
    else
      GIT_STATUS="(%{$fg_bold[green]%}$GIT_WHERE%{$reset_color%}"
    fi
    if [ "$GIT_BEHIND" -gt 0 ]
    then
      GIT_STATUS="$GIT_STATUS%{↓%G%}$GIT_BEHIND"
    fi
    if [ "$GIT_AHEAD" -gt 0 ]
    then
      GIT_STATUS="$GIT_STATUS%{↑%G%}$GIT_AHEAD"
    fi
    GIT_STATUS="$GIT_STATUS|"
    if [ "$GIT_STAGED" -gt 0 ]
    then
      GIT_STATUS="$GIT_STATUS%{$fg[green]%}%{●%G%}$GIT_STAGED%{$reset_color%}"
    fi
    if [ "$GIT_CHANGED" -gt 0 ]
    then
      GIT_STATUS="$GIT_STATUS%{$fg[yellow]%}%{✚%G%}$GIT_CHANGED%{$reset_color%}"
    fi
    if [ "$GIT_UNMERGED" -gt 0 ]
    then
      GIT_STATUS="$GIT_STATUS%{$fg[red]%}%{✖%G%}$GIT_UNMERGED%{$reset_color%}"
    fi
    if [ "$GIT_UNTRACKED" -gt 0 ]
    then
      GIT_STATUS="$GIT_STATUS%{$fg[blue]%}%{…%G%}$GIT_UNTRACKED%{$reset_color%}"
    fi
    if [ "$GIT_CLEAN" -eq 0 ]
    then
      GIT_STATUS="$GIT_STATUS%{$fg_bold[green]%}%{✔%G%}%{$reset_color%}"
    fi
    GIT_STATUS="$GIT_STATUS)"
    echo "$GIT_STATUS"
  fi
}

# History file
HISTFILE=~/.histfile
# The number of lines the shell will keep within one session
HISTSIZE=20480
# The number of lines of history will be saved
SAVEHIST=8192
PROMPT='[%n@%m %~]$ '
RPROMPT='$(git_prompt)'
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
# History search widget with fuzzy matching.
[ -f /usr/share/zsh/plugins/history-search-multi-word/history-search-multi-word.plugin.zsh ] && \
  source /usr/share/zsh/plugins/history-search-multi-word/history-search-multi-word.plugin.zsh

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
autoload -U zcalc
# Search history by entered text
autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

# Autocompletion for kubernetes
if command -v kubectl > /dev/null
then
  source <(kubectl completion zsh)
fi

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
bindkey "^[[A" up-line-or-beginning-search # Up
bindkey "^[[B" down-line-or-beginning-search # Down

alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias zmv='noglob zmv -W'
aliases[=]='noglob zcalc -e'

export PATH=~/.bin-bw:~/.bin:~/.cabal/bin:$PATH

export ALTERNATE_EDITOR=nano
export EDITOR=emacsclient

export GPG_TTY=$(tty)
