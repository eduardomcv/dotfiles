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
		wl-clipboard \
		fzf \
		zoxide \
		bat \
		eza \
		lazygit \
		tldr \
		ttf-jetbrains-mono \
		neovim \
		mise \
		ghostty \
		thunderbird
}
