#!/bin/bash

set -euo pipefail

source ./utils/sudo.sh

check_is_not_sudo

# update system
sudo apt update && sudo apt upgrade -y

# install packages
sudo apt install -y \
  build-essential \
  software-properties-common \
	cmake \
  git \
  stow \
  zsh \
	fd-find \
  ripgrep \
  fzf \
  bat \
  eza \
  tmux

# Add nvim PPA
sudo add-apt-repository -y ppa:neovim-ppa/unstable

# Add wezterm repository
curl -fsSL https://apt.fury.io/wez/gpg.key | sudo gpg --yes --dearmor -o /usr/share/keyrings/wezterm-fury.gpg
echo 'deb [signed-by=/usr/share/keyrings/wezterm-fury.gpg] https://apt.fury.io/wez/ * *' | sudo tee /etc/apt/sources.list.d/wezterm.list
sudo chmod 644 /usr/share/keyrings/wezterm-fury.gpg

# Install packages
sudo apt update
sudo apt install -y neovim wezterm

# Create directory for user binaries
mkdir -p ~/.local/bin

# Install dotfiles
echo Installing dotfiles...
./dotfiles.sh

echo Done.
