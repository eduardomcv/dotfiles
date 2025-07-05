#!/bin/bash

check_rpmfusion() {
	# Check if rpmfusion is enabled
	dnf repolist | grep -q rpmfusion*

	if [[ $? == 1 ]]; then
		# openh264 needs to be explicitly enabled
		sudo dnf config-manager setopt fedora-cisco-openh264.enabled=1

		sudo dnf install \
			https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm \
			https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm
	fi
}

enable_copr() {
	# enable fedora copr repositories
	# yes, it has do be one command at a time.
	# considering putting this on a loop and read repo names from a txt file.
	sudo dnf copr enable -y atim/lazygit
	sudo dnf copr enable -y pgdev/ghostty
	sudo dnf copr enable -y alternateved/eza
}

install_dnf() {
	# update system
	sudo dnf update -y

	check_rpmfusion

	enable_copr

	# install packages
	sudo dnf install -y \
		@development-tools \
		stow \
		lua \
		lua5.1 \
		luarocks \
		jetbrains-mono-fonts-all \
		fd-find \
		ripgrep \
		bat \
		eza \
		fzf \
		zsh \
		tmux \
		lazygit \
		neovim \
		ghostty \
		firefox \
		thunderbird
}
