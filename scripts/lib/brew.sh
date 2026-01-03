#!/bin/bash

check_brew() {
	# Install homebrew if brew command does not exist
	if ! command -v brew >/dev/null 2>&1; then
		/bin/bash -c \
			"$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
	fi
}

install_brew() {
	# Add tap for emacs
 	brew tap d12frosted/emacs-plus

	brew install \
		cmake \
		stow \
		ripgrep \
		fd \
		bat \
		fzf \
		zoxide \
		eza \
		lazygit \
		tealdeer \
		mise \
		emacs-plus@30

	brew install --cask \
		font-iosevka \
		font-iosevka-aile \
		ghostty \
		thunderbird \
		zen

	# Compile latest neovim
	brew install neovim --HEAD
}
