#!/bin/bash

set -euo pipefail

source ./utils/sudo.sh

check_is_sudo

### Requirements
echo Updating system...

apt update || true
apt upgrade -y

echo Installing dependencies...

apt install -y \
  curl \
  wget \
  apt-transport-https \
  ca-certificates \
  gnupg \
  software-properties-common \
  lsb-release

### Add repositories
echo Adding repositories...

# Docker
echo Adding Docker...
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
echo \
  "deb [arch=amd64 signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu \
  $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null

# vscode
echo Adding vscode...
wget -qO- https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > packages.microsoft.gpg
install -o root -g root -m 644 packages.microsoft.gpg /etc/apt/trusted.gpg.d/
sh -c 'echo "deb [arch=amd64,arm64,armhf signed-by=/etc/apt/trusted.gpg.d/packages.microsoft.gpg] https://packages.microsoft.com/repos/code stable main" > /etc/apt/sources.list.d/vscode.list'
rm -f packages.microsoft.gpg

# Spotify
echo Adding Spotify...
curl -sS https://download.spotify.com/debian/pubkey_0D811D58.gpg | apt-key add -
echo "deb http://repository.spotify.com stable non-free" | tee /etc/apt/sources.list.d/spotify.list

# Slack
echo Adding Slack...
curl -L https://packagecloud.io/slacktechnologies/slack/gpgkey | apt-key add -
cat << EOF > /etc/apt/sources.list.d/slacktechnologies_slack.list
deb https://packagecloud.io/slacktechnologies/slack/debian/ jessie main
deb-src https://packagecloud.io/slacktechnologies/slack/debian/ jessie main
EOF

### Main install
echo Installing packages...

apt update || true
apt upgrade -y

apt install -y \
  docker-ce \
  docker-ce-cli \
  containerd.io \
  fonts-firacode \
  exuberant-ctags \
  code \
  spotify-client \
  slack-desktop \
  gimp \
  neovim \
  tmux \
  stow \
  ripgrep

# Setup docker
echo Setting up docker...

# docker-compose
curl -L "https://github.com/docker/compose/releases/download/1.29.2/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
chmod +x /usr/local/bin/docker-compose

# Setup docker for non-root
groupadd docker || true
usermod -aG docker $USER

# Enable docker start on boot
systemctl enable docker.service
systemctl enable containerd.service

# Discord
if [ ! -f /usr/bin/discord ]; then
    echo Adding discord...
    wget -O discord.deb "https://discordapp.com/api/download?platform=linux&format=deb"
    apt install -y ./discord.deb
    rm discord.deb
fi

echo Done
