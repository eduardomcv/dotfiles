#!/bin/bash

set -euo pipefail

source ../utils/sudo.sh

check_is_not_sudo

if ! type yay > /dev/null; then
  echo Please install the yay AUR helper.
  exit 1
fi

echo Installing AUR packages...
yay -Sa --needed --noconfirm \
  spotify \
  visual-studio-code-bin \
  slack-desktop \
  nerd-fonts-fira-code

