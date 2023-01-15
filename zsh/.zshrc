antidote_dir=${ZDOTDIR:-~}/.antidote

# Automatically install antidote
if [[ ! -e $antidote_dir ]] then
  git clone --depth=1 https://github.com/mattmc3/antidote.git $antidote_dir
fi

# source antidote
source $antidote_dir/antidote.zsh

# initialize plugins statically with ${ZDOTDIR:-~}/.zsh_plugins.txt
antidote load

# aliases
alias ls="ls --color=auto -CF"      # if GNU based, show colors with --color
# alias ls="ls -GCF"                # if BSD based, show colors with -G
alias grep='grep --color=auto'
alias l=ls
alias ll="ls -alF"
alias la="ls -A"
alias cat=bat
alias lg="lazygit"
alias vi="env TERM=wezterm nvim"
alias ff="fd -H -t f -E .git"
alias fzf="fzf --preview 'bat --color=always --style=numbers --line-range=:500 {}'"
alias fz="ff | fzf"

# exports
export EDITOR=nvim
export VISUAL=nvim

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
ZSH_AUTOSUGGEST_STRATEGY=(history completion)           # try to find a suggestion from history. if no match is found, try from completion engine
bindkey '^ ' autosuggest-accept                         # accept suggestion with ctrl+space

# starship prompt
eval "$(starship init zsh)"
