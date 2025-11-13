# Exports
export EDITOR=nvim
export VISUAL=nvim

# Aliases
alias l="eza -1 --icons=auto"
alias la="l -a"
alias ll="la -l"
alias lt="l -T"
# alias bat=batcat # bat may be installed as "batcat"
# alias fd=fdfind  # fd may be installed as "fdfind"
alias lg=lazygit
alias v=nvim
alias vi=nvim
alias ff="fd -H -t f -E .git"
alias fz="ff | fzf --preview 'bat --color=always --style=numbers --line-range=:500 {}'"

# Use fzf to search command history
fh() {
	print -z $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed -E 's/ *[0-9]*\*? *//' | sed -E 's/\\/\\\\/g')
}

# Set $PATH so it includes user's private bin if it exists
local_bin_dir="$HOME/.local/bin"

if [ -d $local_bin_dir ]; then
	PATH="$local_bin_dir:$PATH"
fi

# Completion
## ez-compinit must be setup before bootstrapping Antidote
zstyle ':plugin:ez-compinit' 'compstyle' 'zshzoo'
# Shift+Tab to select previous suggestion
zmodload zsh/complist
bindkey -M menuselect '^[[Z' reverse-menu-complete

### Boostrap Antidote plugin manager ###
antidote_dir=${ZDOTDIR:-~}/.antidote

if [[ ! -e $antidote_dir ]]; then
	git clone --depth=1 https://github.com/mattmc3/antidote.git $antidote_dir
fi

source $antidote_dir/antidote.zsh
antidote load
### -------------------------------- ###

# Enable colors
autoload -U colors && colors

# History
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.cache/zsh/history

# Enable editing command with vi editor
autoload -U edit-command-line
zle -N edit-command-line

# Edit line in vi editor with ctrl+e:
bindkey '^e' edit-command-line

# Auto suggestions
## Try to find a suggestion from history. if no match is found, try from completion engine
ZSH_AUTOSUGGEST_STRATEGY=(history completion)
## Accept suggestion with ctrl+space
bindkey '^ ' autosuggest-accept

# Prompt
autoload -U promptinit
promptinit
prompt pure

if command -v mise &>/dev/null; then
	# Activate Mise-en-place
	eval "$(mise activate zsh)"
fi

if command -v zoxide &>/dev/null; then
	# Fast travel with z
	eval "$(zoxide init zsh)"
fi
