#!/bin/bash

set -euo pipefail

source ./scripts/sudo.sh

check_is_not_sudo

if test ! $(which brew); then
  # Install homebrew
  /bin/bash -c \
    "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
fi

# Main install
brew install \
  openssl \
  readline \
  sqlite3 \
  xz \
  zlib \
  wget \
  stow \
  ripgrep \
  fd \
  bat \
  fzf \
  eza \
  lazygit \
  neovim \
  tmux \
	ghostty \
	font-jetbrains-mono
