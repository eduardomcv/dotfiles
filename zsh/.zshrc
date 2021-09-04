# aliases
alias ls="ls --color=auto -F"
alias vi=nvim

# enable colors
autoload -U colors && colors

# setup history
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.cache/zsh/history

zstyle ':completion:*' menu select

# completion
autoload -Uz compinit
zmodload zsh/complist
compinit
_comp_options+=(globdots)   # Include hidden files.

# enable vi mode
bindkey -v

# shift+tab to select previous suggestion
bindkey -M menuselect '^[[Z' reverse-menu-complete

# syntax highlighting
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

