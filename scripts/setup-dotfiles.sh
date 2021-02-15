#!/bin/bash

set -euo pipefail

source ./utils/sudo.sh

check_is_not_sudo

rsync --quiet \
  --exclude ".git/" \
  --exclude "utils/" \
  --exclude "install.sh" \
  --exclude "README.md" \
  --exclude ".bashrc" \
  --exclude "git-prompt.txt" \
  --exclude "logid.cfg" \
  -avh --no-perms . ~

# WIP
# setup git bash prompt
cat git-prompt.txt >> ~/.bashrc
