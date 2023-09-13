#!/bin/bash

set -euo pipefail

source ./utils/sudo.sh

check_is_not_sudo

if test ! $(which brew); then
  # Install homebrew
  /bin/bash -c \
  "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
fi

# Taps
brew tap homebrew/cask-fonts

# Main install
brew install \
  stow \
  wget \
  ripgrep \
  fd \
  bat \
  exa \
  fzf \
  starship \
  lazygit \
  neovim \
  alacritty \
  tmux \
  font-fira-code-nerd-font \
  firefox

