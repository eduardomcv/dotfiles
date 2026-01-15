#!/bin/bash

check_brew() {
    # Install homebrew if brew command does not exist
    if ! command -v brew >/dev/null 2>&1; then
        /bin/bash -c \
            "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
    fi
}

install_brew() {
    # Add tap for emacs
    brew tap d12frosted/emacs-plus

    brew install \
        cmake \
        libvterm \
        stow \
        ripgrep \
        fd \
        bat \
        fzf \
        zoxide \
        eza \
        tealdeer \
        vim \
        mise \
        emacs-plus

    brew install --cask \
        font-iosevka \
        font-iosevka-aile \
        font-symbols-only-nerd-font \
        thunderbird \
        zen
}
