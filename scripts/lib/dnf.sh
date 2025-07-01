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

install_dnf() {
	# update system
	sudo dnf update -y

	check_rpmfusion

	sudo dnf copr enable -y pgdev/ghostty

	# install packages
	sudo dnf install -y \
		@development-tools \
		stow \
		zsh \
		fd-find \
		ripgrep \
		fzf \
		bat \
		tmux \
		lua \
		lua5.1 \
		luarocks \
		neovim \
		jetbrains-mono-fonts-all \
		ghostty
}
