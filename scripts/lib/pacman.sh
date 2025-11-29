#!/bin/bash

install_pacman() {
	# update system
	sudo pacman -Syu --noconfirm

	# install packages
	sudo pacman -S --noconfirm --needed \
		base-devel \
		zip \
		unzip \
		git \
		stow \
		zsh \
		fd \
		ripgrep \
		fzf \
		zoxide \
		bat \
		eza \
		lazygit \
		tealdeer \
		ttf-jetbrains-mono \
		neovim \
		mise \
		ghostty \
		thunderbird
}
