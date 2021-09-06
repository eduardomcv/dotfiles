# aliases
# alias ls="ls --color=auto -F"             # on linux, show colors with --color=auto
alias ls="ls -GF"                           # on macOS, show colors with -G
alias ll="ls -l"
alias la="ls -la"
alias vi=nvim

# exports
export VIMRC=~/.config/nvim/init.vim        # easy vim config access

# enable colors
autoload -U colors && colors

# setup history
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.cache/zsh/history

# completion
autoload -Uz compinit
zmodload zsh/complist
zstyle ':completion:*' menu select
compinit
_comp_options+=(globdots)   # Include hidden files.

# enable vi mode
bindkey -v

# shift+tab to select previous suggestion
bindkey -M menuselect '^[[Z' reverse-menu-complete

# syntax highlighting
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

