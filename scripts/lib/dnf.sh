#!/bin/bash

check_rpmfusion() {
    echo "Checking if RPM Fusion is enabled..."
    dnf repolist | grep -q rpmfusion-free

    if [[ $? == 1 ]]; then
        sudo dnf config-manager setopt fedora-cisco-openh264.enabled=1

        sudo dnf install -y \
            https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm \
            https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm
    else
        echo "RPM Fusion is already enabled!"
    fi
}

enable_copr() {
    local COPR_NAMES=(
     "atim/lazygit"
     "alternateved/eza"
     "sneexy/zen-browser"
     "jdxcode/mise"
     "peterwu/iosevka"
     "che/nerd-fonts"
    )

    echo "Checking if COPRs are enabled..."
    dnf copr list | grep -q "$COPR_NAMES[-1]"


    if [[ $? == 1 ]]; then
        for name in "${COPR_NAMES[@]}"
        do
            sudo dnf copr enable -y "$name"
        done
    else
        echo "COPRs are already enabled!"
    fi
}

install_dnf() {
    # update system
    sudo dnf update -y \
        && check_rpmfusion \
        && enable_copr \
        && sudo dnf install -y \
            @development-tools \
            autoconf \
            make \
            bzip2 \
            openssl-devel \
            libyaml-devel \
            libffi-devel \
            readline-devel \
            gdbm-devel \
            ncurses-devel \
            perl-FindBin \
            zlib-ng-compat-devel \
            cmake \
            gcc-c++ \
            libtool \
            libvterm-devel \
            wl-clipboard \
            gnome-tweaks \
            gnome-extensions-app \
            gnome-themes-extra \
            zsh \
            stow \
            fd-find \
            ripgrep \
            bat \
            fzf \
            tldr \
            zoxide \
            emacs \
            mise \
            lazygit \
            eza \
            zen-browser \
            iosevka-fonts \
            iosevka-term-fonts \
            nerd-fonts
}
