# Check and enable Homebrew
if [[ -x /home/linuxbrew/.linuxbrew/bin/brew ]]; then
	eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
elif [[ -x /opt/homebrew/bin/brew ]]; then
	eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# Add Homebrew's completions
if type brew &>/dev/null; then
	FPATH="$(brew --prefix)/share/zsh-completions:${FPATH}"
fi

# Activate Mise-en-place
if command -v mise &>/dev/null; then
	eval "$(mise activate zsh)"
fi

# Fast travel with z
if command -v zoxide &>/dev/null; then
	eval "$(zoxide init zsh)"
fi

# ez-compinit must be setup before bootstrapping Antidote plugin manager
zstyle ':plugin:ez-compinit' 'compstyle' 'zshzoo'
zmodload zsh/complist
# Shift+Tab to select previous suggestion
bindkey -M menuselect '^[[Z' reverse-menu-complete

# Boostrap Antidote plugin manager
antidote_dir=${ZDOTDIR:-~}/.antidote
if [[ ! -e $antidote_dir ]]; then
	git clone --depth=1 https://github.com/mattmc3/antidote.git $antidote_dir
fi
source $antidote_dir/antidote.zsh
antidote load

# Enable colors
autoload -U colors && colors

# History config
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.cache/zsh/history

# Enable vi mode
autoload -U edit-command-line
zle -N edit-command-line
# Edit line in vi editor with ctrl+e:
bindkey '^e' edit-command-line

# Autosuggest strategy:
# Try to find a suggestion from history. If no match is found, try from completion engine
ZSH_AUTOSUGGEST_STRATEGY=(history completion)
# Accept suggestion with ctrl+space
bindkey '^ ' autosuggest-accept

# Setup pure prompt
autoload -U promptinit
promptinit
prompt pure

# Aliases
alias l="eza -1 --icons=auto"
alias la="l -a"
alias ll="la -l"
alias lt="l -T"
alias cat=bat
# alias cat=batcat # bat may be installed as "batcat"
# alias fd=fdfind  # fd may be installed as "fdfind"
alias lg=lazygit
alias v=nvim
alias vi=nvim
alias ff="fd -H -t f -E .git"
alias fz="ff | fzf --preview 'bat --color=always --style=numbers --line-range=:500 {}'"

# Search command history with fzf
fh() {
	print -z "$( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed -E 's/ *[0-9]*\*? *//' | sed -E 's/\\/\\\\/g')"
}
