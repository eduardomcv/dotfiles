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
		tmux \
		lazygit \
		lua51 \
		luarocks \
		ttf-jetbrains-mono \
		neovim \
		ghostty
}
