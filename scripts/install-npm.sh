#!/bin/bash

# Run this after installing Node.js through nvm.
# Installs dependencies and language servers for neovim

set -euo pipefail

if ! command -v npm >/dev/null 2>&1; then
	echo "npm is not installed!"
	exit 1
fi

npm install -g \
	prettier \
	@vtsls/language-server \
	@olrtg/emmet-language-server \
	bash-language-server \
	yaml-language-server \
	vscode-langservers-extracted
