#!/bin/bash

set -euo pipefail

source ../utils/sudo.sh

check_is_sudo

echo Installing packages...
pacman -Syu --needed --noconfirm \
  base-devel \
  git \
  docker \
  docker-compose \
  ttf-fira-code \
  ctags \
  gimp \
  neovim \
  tmux \
  stow \
  discord \
  ripgrep

