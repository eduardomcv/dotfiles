#!/bin/bash

set -euo pipefail

source ./utils/sudo.sh

check_is_not_sudo

# update system
sudo pacman -Syu --noconfirm

# install dependencies
sudo pacman -S --noconfirm --needed \
  base-devel \
  power-profiles-daemon \
  system-config-printer \
  noto-fonts-cjk \
  noto-fonts-emoji \
  firewalld \
  stow \
  zip \
  unzip \
  zsh \
  openssh \
  ripgrep \
  fzf \
  fd \
  bat \
  eza \
  wl-clipboard \
  git \
  lazygit \
  ttf-jetbrains-mono-nerd \
  tmux \
  wezterm \
  neovim \
  flatpak \
  qemu \
  libvirt \
  edk2-ovmf \
  virt-manager \
  dnsmasq \
  ebtables \
  swtpm \
  firefox \
  steam \
  spotify-launcher

# create directory for user binaries
mkdir -p ~/.local/bin

# install dotfiles
echo Installing dotfiles...
./dotfiles.sh

echo Done.
