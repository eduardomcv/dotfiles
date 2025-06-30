#!/bin/bash

set -euo pipefail

source ./scripts/sudo.sh

check_is_not_sudo

# update system
sudo apt update && sudo apt upgrade -y

# install packages
sudo apt install -y \
  build-essential \
  software-properties-common \
  git \
  stow \
  zsh \
	fd-find \
  ripgrep \
  fzf \
  bat \
  eza \
  tmux

# Install lazygit
LAZYGIT_VERSION=$(curl -s "https://api.github.com/repos/jesseduffield/lazygit/releases/latest" | \grep -Po '"tag_name": *"v\K[^"]*')
curl -Lo lazygit.tar.gz "https://github.com/jesseduffield/lazygit/releases/download/v${LAZYGIT_VERSION}/lazygit_${LAZYGIT_VERSION}_Linux_x86_64.tar.gz"
tar xf lazygit.tar.gz lazygit
sudo install lazygit -D -t /usr/local/bin/

# Add nvim PPA
sudo add-apt-repository -y ppa:neovim-ppa/unstable

# Add Spotify repository
curl -sS https://download.spotify.com/debian/pubkey_C85668DF69375001.gpg | sudo gpg --dearmor --yes -o /etc/apt/trusted.gpg.d/spotify.gpg
echo "deb https://repository.spotify.com stable non-free" | sudo tee /etc/apt/sources.list.d/spotify.list

# Install packages
sudo apt update
sudo apt install -y neovim spotify-client

# Create directory for user binaries
mkdir -p ~/.local/bin

# Install dotfiles
echo Installing dotfiles...
./dotfiles.sh

echo Done.
