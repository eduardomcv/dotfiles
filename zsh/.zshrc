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

# Activate worktrunk
if command -v wt >/dev/null 2>&1; then
	eval "$(command wt config shell init zsh)"
fi

# Load aliases
source "$HOME/.zsh_aliases"

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

# History
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.cache/zsh/history

# Enable vi mode
autoload -U edit-command-line
zle -N edit-command-line
# Edit line in vi editor with ctrl+e:
bindkey '^e' edit-command-line

# Try to find a suggestion from history. If no match is found, try from completion engine
ZSH_AUTOSUGGEST_STRATEGY=(history completion)
# Accept suggestion with ctrl+space
bindkey '^ ' autosuggest-accept

# Pure prompt
autoload -U promptinit
promptinit
prompt pure
