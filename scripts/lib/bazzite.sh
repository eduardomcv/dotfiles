#!/bin/bash

install_bazzite() {
	brew install --cask font-jetbrains-mono

	brew install \
		stow \
		ripgrep \
		fd \
		bat \
		fzf \
		eza \
		lazygit \
		neovim \
		tmux

	flatpak install -y \
		app.zen_browser.zen \
		org.mozilla.Thunderbird
}
