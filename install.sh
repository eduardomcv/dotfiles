#!/bin/bash

set -euo pipefail

source ./utils/sudo.sh

check_is_not_sudo

THINGS_TO_STOW=( \
    git \
    zsh \
    wezterm \
    neovim \
    i3 \
    tmux
)

for thing in ${THINGS_TO_STOW[@]}; do
    stow -vDt ~ $thing
    stow -vt ~ $thing
done

echo Done.

