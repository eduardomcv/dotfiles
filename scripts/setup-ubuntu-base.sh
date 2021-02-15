#!bin/bash

set -euo pipefail

source ./utils/sudo.sh

check_is_sudo

# "lsb_release" prints distro-specific info.
# Because of that, we need to specify which release we're using as a base.
RELEASE_NAME=focal

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
  gnupg-agent \
  software-properties-common

### Add repositories
echo Adding repositories...

# Docker
echo Adding Docker...
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | apt-key add -
sudo add-apt-repository \
  "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
  $RELEASE_NAME \
  stable"

# vscode
echo Adding vscode...
wget -qO- https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > packages.microsoft.gpg
install -o root -g root -m 644 packages.microsoft.gpg /etc/apt/trusted.gpg.d/
sh -c 'echo "deb [arch=amd64 signed-by=/etc/apt/trusted.gpg.d/packages.microsoft.gpg] https://packages.microsoft.com/repos/vscode stable main" > /etc/apt/sources.list.d/vscode.list'
rm packages.microsoft.gpg

# Spotify
echo Adding Spotify...
curl -sS https://download.spotify.com/debian/pubkey_0D811D58.gpg | apt-key add -
echo "deb http://repository.spotify.com stable non-free" | tee /etc/apt/sources.list.d/spotify.list

# Slack
echo Adding Slack...
curl -fsSL https://packagecloud.io/slacktechnologies/slack/gpgkey | apt-key add -
add-apt-repository "deb https://packagecloud.io/slacktechnologies/slack/debian/ jessie main"

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
  neovim

# Setup docker
echo Setting up docker...

# docker-compose
curl -L "https://github.com/docker/compose/releases/download/1.28.2/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
chmod +x /usr/local/bin/docker-compose

# Setup docker for non-root
groupadd docker || true
usermod -aG docker $USER

# Enable docker start on boot
systemctl enable docker.service
systemctl enable containerd.service

# Apply group changes
newgrp docker

# Discord
if [ ! -f /usr/bin/discord ]; then
    echo Adding discord...
    wget -O discord.deb "https://discordapp.com/api/download?platform=linux&format=deb"
    apt install -y ./discord.deb
    rm discord.deb
fi
echo 

echo Done
