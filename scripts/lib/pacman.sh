#!/bin/bash

install_pacman() {
	# update system
	sudo pacman -Syu --noconfirm

	# install packages
	sudo pacman -S --noconfirm \
		base-devel \
		zip \
		unzip \
		git \
		stow \
		zsh \
		fd \
		ripgrep \
		fzf \
		bat \
		eza \
		lazygit \
		tealdeer \
		ttf-jetbrains-mono \
		stylua \
		shfmt \
		lua-language-server \
		neovim \
		ghostty \
		thunderbird
}
