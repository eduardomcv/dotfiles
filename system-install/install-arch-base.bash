#!/bin/bash

set -euo pipefail

source ../utils/sudo.sh

check_is_sudo

echo Installing packages...
pacman -Syu --needed --noconfirm \
  base-devel \
  zsh \
  zsh-syntax-highlighting \
  zsh-autosuggestions \
  git \
  stow \
  ripgrep \
  neovim \
  tmux \
  gimp \
  ctags \
  discord \
  docker \
  docker-compose

