#!/bin/bash

set -euo pipefail

source ./utils/sudo.sh

check_is_not_sudo

sudo apt update -y
sudo apt upgrade -y

# install dependencies
sudo apt install -y \
	build-essential \
	fuse3 \
	libfuse2 \
	git \
	stow \
	curl \
	wget \
	zsh \
	zip \
	unzip \
	tar \
	ripgrep \
	bat \
	fd-find \
	exa \
	fzf \
	tmux \
	flatpak \
	xclip

# create directory for user binaries
mkdir -p ~/.local/bin

# add flathub repository
flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo

# install starship prompt
curl -sS https://starship.rs/install.sh | sh -s -- -y

# install latest neovim stable AppImage
echo Installing neovim...
nvim_bin_path=~/.local/bin/nvim
curl -sSL https://github.com/neovim/neovim/releases/download/stable/nvim.appimage >$nvim_bin_path
chmod +x $nvim_bin_path

# install lazygit
echo Installing lazygit...
LAZYGIT_VERSION=$(curl -Ss "https://api.github.com/repos/jesseduffield/lazygit/releases/latest" | grep -Po '"tag_name": "v\K[^"]*')
curl -LSso lazygit.tar.gz "https://github.com/jesseduffield/lazygit/releases/latest/download/lazygit_${LAZYGIT_VERSION}_Linux_x86_64.tar.gz"
tar xf lazygit.tar.gz lazygit
sudo install lazygit /usr/local/bin
rm lazygit*

echo Done.
