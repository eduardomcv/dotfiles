#!/bin/bash

set -euo pipefail

source ./utils/sudo.sh

check_is_not_sudo

THINGS_TO_STOW=( \
    git \
    zsh \
    alacritty \
    neovim \
    tmux
)

for thing in ${THINGS_TO_STOW[@]}; do
    if [[ "$#" == 0 || "$1" == "uninstall" ]]; then
        stow -vDt ~ $thing
    fi

    if [[ "$#" == 0 || $1 == "install" ]]; then
        stow -vt ~ $thing
    fi
done

echo Done.

