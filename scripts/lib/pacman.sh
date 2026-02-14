#!/bin/bash

install_pacman() {
    # update system
    sudo pacman -Syu --noconfirm

    # install packages
    sudo pacman -S --noconfirm --needed \
        base-devel \
        cmake \
        libffi \
        libyaml \
        openssl \
        zip \
        unzip \
        git \
        stow \
        zsh \
        fd \
        ripgrep \
        wl-clipboard \
        fzf \
        zoxide \
        bat \
        eza \
        mise \
        tealdeer \
        lazygit \
        emacs-wayland
}
