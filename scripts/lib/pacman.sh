#!/bin/bash

install_pacman() {
	# update system
	sudo pacman -Syu

	# install packages
	sudo pacman -S --noconfirm \
		base-devel \
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
		neovim \
		ttf-jetbrains-mono
}
