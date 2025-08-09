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
		openssl \
		readline \
		sqlite3 \
		xz \
		zlib \
		wget \
		stow \
		ripgrep \
		fd \
		bat \
		fzf \
		eza \
		lazygit \
		neovim \
		tmux \
		ghostty \
		font-jetbrains-mono \
		thunderbird \
		firefox
}
