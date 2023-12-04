#!/bin/bash

set -euo pipefail

source ./utils/sudo.sh

check_is_not_sudo

sudo apt update -y

sudo apt upgrade -y

sudo apt install -y \
	build-essential \
	fuse \
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
	xclip \
	tmux

# Create directory for user binaries
mkdir -p ~/.local/bin

# install starship prompt
curl -sS https://starship.rs/install.sh | sh

# install latest neovim stable AppImage
echo Installing neovim...
curl -sSL https://github.com/neovim/neovim/releases/download/stable/nvim.appimage -o ~/.local/bin/nvim
chmod +x ~/.local/bin/nvim

# install lazygit
echo Installing lazygit...
LAZYGIT_VERSION=$(curl -Ss "https://api.github.com/repos/jesseduffield/lazygit/releases/latest" | grep -Po '"tag_name": "v\K[^"]*')
curl -LSso lazygit.tar.gz "https://github.com/jesseduffield/lazygit/releases/latest/download/lazygit_${LAZYGIT_VERSION}_Linux_x86_64.tar.gz"
tar xf lazygit.tar.gz lazygit
sudo install lazygit /usr/local/bin
rm lazygit*

echo Done.
