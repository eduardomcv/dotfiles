#!/bin/bash

set -euo pipefail

source ./utils/sudo.sh

check_is_sudo

pacman -Syu --noconfirm \
  base-devel \
  man-db \
  git \
  stow \
  curl \
  wget \
  i3-wm \
  i3lock \
  polybar \
  rofi \
  ttf-firacode-nerd \
  adobe-source-han-sans-otc-fonts \
  adobe-source-han-serif-otc-fonts \
  blueman \
  networkmanager \
  network-manager-applet \
  lxsession-gtk3 \
  dunst \
  maim \
  htop \
  imagemagick \
  feh \
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
  xclip \
  firefox \
  alacritty \
  thunar \
  thunar-archive-plugin \
  thunar-media-tags-plugin \
  thunar-volman \
  ristretto \
  vlc \
  mate-calc \
  flatpak \
  steam \
  discord
