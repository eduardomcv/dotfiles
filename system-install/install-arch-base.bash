#!/bin/bash

set -euo pipefail

source ../utils/sudo.sh

check_is_sudo

echo Installing packages...
pacman -Syu --needed --noconfirm \
  base-devel \
  zsh \
  zsh-syntax-highlighting \
  git \
  stow \
  ripgrep \
  neovim \
  tmux \
  gimp \
  ctags \
  ttf-fira-code \
  discord \
  docker \
  docker-compose

