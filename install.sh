#!/usr/bin/env bash

set -e
set -o pipefail

check_is_sudo() {
  if [ "$(id -u)" -ne 0 ]; then
    echo "Please run as root."
    exit
  fi
}

check_isnt_sudo() {
  if [ "$(id -u)" -eq 0 ]; then
    echo "Please run without root."
    exit
  fi
}

setup_dotfiles() {
  rsync --quiet \
    --exclude ".git/" \
    --exclude "install.sh" \
    --exclude "README.md" \
    --exclude ".bashrc" \
    -avh --no-perms . ~

  # cat .bashrc >> ~/.bashrc
}

setup_system() {
  # pre adding repositories requirements
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
  ### no repo available for focal fossa yet
  # sudo add-apt-repository \
  #   "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
  #   $(lsb_release -cs) \
  #   stable"
  add-apt-repository \
    "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
    eoan \
    stable"

  ## vscode
  curl https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > packages.microsoft.gpg
  install -o root -g root -m 644 packages.microsoft.gpg /usr/share/keyrings/
  sh -c 'echo "deb [arch=amd64 signed-by=/usr/share/keyrings/packages.microsoft.gpg] https://packages.microsoft.com/repos/vscode stable main" > /etc/apt/sources.list.d/vscode.list'

  ## spotify
  curl -sS https://download.spotify.com/debian/pubkey.gpg | apt-key add - 
  echo "deb http://repository.spotify.com stable non-free" | tee /etc/apt/sources.list.d/spotify.list

  ## slack
  curl -fsSL https://packagecloud.io/slacktechnologies/slack/gpgkey | apt-key add -
  add-apt-repository "deb https://packagecloud.io/slacktechnologies/slack/debian/ jessie main"

  # main install
  apt update
  apt upgrade -y

  apt install -y \
    docker-ce \
    docker-ce-cli \
    containerd.io \
    fonts-firacode \
    exuberant-ctags \
    gnome-tweaks \
    code \
    spotify-client \
    slack-desktop \
    gimp

  # docker-compose
  curl -L "https://github.com/docker/compose/releases/download/1.25.5/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
  chmod +x /usr/local/bin/docker-compose

  # setup docker for non-root
  usermod -aG docker $USER
  systemctl enable docker

  # discord
  if [ ! -f /usr/bin/discord ]; then
      wget -O discord.deb "https://discordapp.com/api/download?platform=linux&format=deb"
      apt install -y ./discord.deb
      rm discord.deb
  fi

  # setup git bash prompt
  cat git-prompt.txt >> ~/.bashrc
}

setup_node() {
  # Node Version Manager
  curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.3/install.sh | bash
  # install latest lts version
  nvm install --lts --latest-npm
}

setup_python() {
  # pyenv installer
  curl -L https://github.com/pyenv/pyenv-installer/raw/master/bin/pyenv-installer | bash
  printf "\\nexport PATH=\"$HOME/.pyenv/bin:$PATH\"\\neval \"\$(pyenv init -)\"\\neval \"\$(pyenv virtualenv-init -)\"\\n" >> ~/.bashrc

  pyenv install 3.6.10
  pyenv global 3.6.10
}

main() {
  local cmd=$1

  if [[ $cmd == "dotfiles" ]]; then
    check_isnt_sudo
    setup_dotfiles
  elif [[ $cmd == "system" ]]; then
    check_is_sudo
    setup_system
  elif [[ $cmd == "node" ]]; then
    check_isnt_sudo
    setup_python
  elif [[ $cmd == "python" ]]; then
    check_isnt_sudo
    setup_python
  else
    echo "please specify what to install"
  fi
}

main "$@"
