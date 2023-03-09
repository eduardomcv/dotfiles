#!/bin/bash

set -euo pipefail

source ./utils/sudo.sh

check_is_not_sudo

THINGS_TO_STOW=( \
    git \
    zsh \
    wezterm \
    neovim \
    vscode \
    i3
)

for thing in ${THINGS_TO_STOW[@]}; do
    stow -vDt ~ $thing
    stow -vt ~ $thing
done

echo Done.

