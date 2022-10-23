#
# Simple config for zshell.
#
# Includes:
#   - aliases
#   - a simple prompt with version control
#   - syntax highlighting
#   - vi mode and vi keys
#   - completions
#   - suggestions
#

# aliases
alias ls="ls --color=auto -CF"      # if GNU based, show colors with --color
# alias ls="ls -GCF"                # if BSD based, show colors with -G
alias grep='grep --color=auto'
alias l=ls
alias ll="ls -alF"
alias la="ls -A"
alias vi="env TERM=wezterm nvim"

# exports
export EDITOR=nvim
export VISUAL=nvim
export VIMRC=~/.config/nvim/init.lua

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

# history
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.cache/zsh/history

# completion
autoload -Uz compinit
zmodload zsh/complist
zstyle ':completion:*' menu select
compinit -d ~/.cache/zsh/compdump
_comp_options+=(globdots)           # Include hidden files

# enable colors
autoload -U colors && colors

# version control
autoload -Uz vcs_info
zstyle ':vcs_info:git*' formats "%F{yellow}(%b)%f %m%u%c"                                   # format vcs prompt
zstyle ':vcs_info:git*' actionformats "%F{yellow}(%b%F{blue}|%F{red}%a%F{yellow})%f %u%c"   # change vcs formatting to show certain actions i.e. merge
precmd() { vcs_info }

# change prompt
setopt prompt_subst
PROMPT='%F{blue}%B%~%b%f ${vcs_info_msg_0_}% > '

# enable editing command with vi editor
autoload -U edit-command-line
zle -N edit-command-line

# Edit line in vi editor with ctrl+e:
bindkey '^e' edit-command-line

# enable vi mode
bindkey -v

# enable vi keys in completion menu by using ctrl
bindkey -M menuselect '^h' vi-backward-char
bindkey -M menuselect '^k' vi-up-line-or-history
bindkey -M menuselect '^l' vi-forward-char
bindkey -M menuselect '^j' vi-down-line-or-history

# shift+tab to select previous suggestion
bindkey -M menuselect '^[[Z' reverse-menu-complete

# auto suggestions
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh   # source plugin (path may vary per OS)
ZSH_AUTOSUGGEST_STRATEGY=(history completion)                               # try to find a suggestion from history. if no match is found, try from completion engine
bindkey '^ ' autosuggest-accept                                             # accept suggestion with ctrl+space

# syntax highlighting (must be loaded last)
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh   # path may vary per OS

