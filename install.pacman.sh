#!/bin/bash

set -euo pipefail

source ./utils/sudo.sh

check_is_not_sudo

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
	ttf-jetbrains-mono \
	discord \
	steam \
	lutris

# Create directory for user binaries
mkdir -p ~/.local/bin

# Install dotfiles
echo Installing dotfiles...
./dotfiles.sh

echo Done.
