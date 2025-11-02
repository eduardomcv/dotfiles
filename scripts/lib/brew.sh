#!/bin/bash

check_brew() {
	# Install homebrew if brew command does not exist
	if ! command -v brew >/dev/null 2>&1; then
		/bin/bash -c \
			"$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
	fi
}

install_brew() {
	brew install \
		stow \
		ripgrep \
		fd \
		bat \
		fzf \
		eza \
		lazygit \
		tealdeer \
		stylua \
		lua-language-server \
		shfmt \
		neovim \
		ghostty \
		font-jetbrains-mono \
		thunderbird \
		zen
}
