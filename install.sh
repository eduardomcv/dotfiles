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

echo Done.
