#!/usr/bin/env bash

set -e
set -o pipefail

check_is_sudo() {
  if [ "$(id -u)" -ne 0 ]; then
    echo "Please run as root."
    exit
  fi
}

check_is_not_sudo() {
  if [ "$(id -u)" -eq 0 ]; then
    echo "Please run as non-root."
    exit
  fi
}

setup_dotfiles() {
  rsync --quiet \
    --exclude ".git/" \
    --exclude "install.sh" \
    --exclude "README.md" \
    --exclude ".bashrc" \
    --exclude "git-prompt.txt" \
    -avh --no-perms . ~

  # setup git bash prompt
  cat git-prompt.txt >> ~/.bashrc
}

setup_system() {
  # requirements
  apt update || true
  apt upgrade -y
  
  apt install -y \
    curl \
    wget \
    make \
    apt-transport-https \
    ca-certificates \
    gnupg-agent \
    software-properties-common \
    build-essential \
    libssl-dev \
    zlib1g-dev \
    libbz2-dev \
    libreadline-dev \
    libsqlite3-dev \
    llvm \
    libncurses5-dev \
    xz-utils \
    tk-dev \
    libxml2-dev \
    libxmlsec1-dev \
    libffi-dev \
    liblzma-dev
  
  # add repositories
  ## docker
  curl -fsSL https://download.docker.com/linux/ubuntu/gpg | apt-key add -
  sudo add-apt-repository \
    "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
    focal \
    stable"

  ## yarn
  curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
  echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list

  ## vscode
  curl https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > packages.microsoft.gpg
  install -o root -g root -m 644 packages.microsoft.gpg /usr/share/keyrings/
  sh -c 'echo "deb [arch=amd64 signed-by=/usr/share/keyrings/packages.microsoft.gpg] https://packages.microsoft.com/repos/vscode stable main" > /etc/apt/sources.list.d/vscode.list'
  rm packages.microsoft.gpg

  ## chrome
  curl -fsSL https://dl.google.com/linux/linux_signing_key.pub | apt-key add -
  echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" > /etc/apt/sources.list.d/google-chrome.list

  ## spotify
  curl -sS https://download.spotify.com/debian/pubkey.gpg | apt-key add - 
  echo "deb http://repository.spotify.com stable non-free" | tee /etc/apt/sources.list.d/spotify.list

  ## slack
  curl -fsSL https://packagecloud.io/slacktechnologies/slack/gpgkey | apt-key add -
  add-apt-repository "deb https://packagecloud.io/slacktechnologies/slack/debian/ jessie main"

  # main install
  apt update || true
  apt upgrade -y

  apt install -y \
    docker-ce \
    docker-ce-cli \
    containerd.io \
    fonts-firacode \
    exuberant-ctags \
    yarn \
    code \
    google-chrome-stable \
    spotify-client \
    slack-desktop \
    gimp \
    vim

  # docker-compose
  curl -L "https://github.com/docker/compose/releases/download/1.25.5/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
  chmod +x /usr/local/bin/docker-compose

  # setup docker for non-root
  groupadd docker || true
  usermod -aG docker $USER
  systemctl enable docker

  # discord
  if [ ! -f /usr/bin/discord ]; then
      wget -O discord.deb "https://discordapp.com/api/download?platform=linux&format=deb"
      apt install -y ./discord.deb
      rm discord.deb
  fi
}

apply_group_changes() {
  newgrp docker
}

setup_node() {
  # Node Version Manager
  curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.3/install.sh | bash
}

setup_python() {
  # pyenv installer
  curl -L https://github.com/pyenv/pyenv-installer/raw/master/bin/pyenv-installer | bash

  SOURCE_STRING="\\nexport PATH=\"$HOME/.pyenv/bin:$PATH\"\\neval \"\$(pyenv init -)\"\\neval \"\$(pyenv virtualenv-init -)\"\\n"
  printf "${SOURCE_STRING~}" >> ~/.bashrc
}

print_help_message() {
  echo "system -- setup system"
  echo "dotfiles -- setup dotfiles in home directory"
  echo "node -- setup node"
  echo "python -- setup python"
  echo "apply-group-changes -- applies changes made to groups (so docker can be used as non-root)"
  echo "help -- print help message"
}

main() {
  local cmd=$1

  if [[ $cmd == "dotfiles" ]]; then
    check_is_not_sudo
    setup_dotfiles
  elif [[ $cmd == "system" ]]; then
    check_is_sudo
    setup_system
  elif [[ $cmd == "node" ]]; then
    check_is_not_sudo
    setup_node
  elif [[ $cmd == "python" ]]; then
    check_is_not_sudo
    setup_python
  elif [[ $cmd == "apply-group-changes" ]]; then
    check_is_not_sudo
    apply_group_changes
  elif [[ $cmd == "help" ]]; then
    print_help_message
  else
    echo -e "please specify what to install"
    print_help_message
  fi
}

main "$@"
