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
        zlib-ng-compat \
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
        lazygit \
        mise \
        ghostty \
        neovim-nightly-bin \
        ttc-iosevka \
        ttc-iosevka-aile \
        ttf-nerd-fonts-symbols-mono \
        emacs-wayland \
        thunderbird
}
