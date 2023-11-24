#!/bin/bash

set -euo pipefail

source ./utils/sudo.sh

check_is_sudo

pacman -Syu --noconfirm \
  base-devel \
  git \
  stow \
  curl \
  wget \
  ttf-firacode-nerd \
  htop \
  zsh \
  zip \
  unzip \
  starship \
  ripgrep \
  bat \
  fd \
  exa \
  fzf \
  lazygit \
  neovim \
  alacritty \
  tmux \
  firefox
