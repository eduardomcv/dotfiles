#!/usr/bin/env bash
# shellcheck disable=SC1090

set -e
set -o pipefail

check_sudo() {
  if [ "$(id -u)" -ne 0 ]; then
    echo "Please run as root."
    exit
  fi
}

dotfiles() {
  rsync --quiet \
    --exclude ".git/" \
    --exclude "install" \
    --exclude "README.md" \
    --exclude ".bashrc" \
    -avh --no-perms . ~

  # cat .bashrc >> ~/.bashrc
}

tools() {
  apt update || true
  apt upgrade -y
  
  apt install -y \
    curl
    fonts-firacode
  
  # Node Version Manager
  curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.3/install.sh | bash
}

main() {
  local cmd=$1

  if [[ $cmd == "dotfiles" ]]; then
    dotfiles
  elif [[ $cmd == "tools" ]]; then
    check_sudo
    tools
  else
    echo "please specify"
  fi
}

main "$@"
