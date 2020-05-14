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
  # pre adding repositories requirements
  apt update || true
  apt upgrade -y
  
  apt install -y \
    curl \
    apt-transport-https \
    ca-certificates \
    gnupg-agent \
    software-properties-common
  
  # add repositories
  ## docker
  curl -fsSL https://download.docker.com/linux/ubuntu/gpg | apt-key add -
  ### no repo available for focal fossa yet
  # sudo add-apt-repository \
  #   "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
  #   $(lsb_release -cs) \
  #   stable"
  sudo add-apt-repository \
    "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
    eoan \
    stable"

  # post add-repositories install
  apt update || true
  apt upgrade -y

  apt install -y \
    docker-ce \
    docker-ce-cli \
    containerd.io
  
  # Node Version Manager
  curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.3/install.sh | bash

  # docker-compose
  curl -L "https://github.com/docker/compose/releases/download/1.25.5/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
  chmod +x /usr/local/bin/docker-compose

  # setup docker for non-root
  usermod -aG docker $USER
  systemctl enable docker
}

main() {
  local cmd=$1

  if [[ $cmd == "dotfiles" ]]; then
    dotfiles
  elif [[ $cmd == "tools" ]]; then
    check_sudo
    tools
  else
    echo "please specify what to install"
  fi
}

main "$@"
