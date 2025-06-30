#!/bin/bash

set -euo pipefail

source ./scripts/sudo.sh

check_is_not_sudo

THINGS_TO_STOW=(
	git
	zsh
	neovim
	tmux
	ghostty
)

INSTALL=false
UNINSTALL=false

if [[ "$#" == 0 || "$1" == "install" ]]; then
	INSTALL=true
fi

if [[ "$#" == 0 || "$1" == "uninstall" ]]; then
	UNINSTALL=true
fi

for thing in ${THINGS_TO_STOW[@]}; do
	if [[ "$UNINSTALL" == true ]]; then
		stow -vDt ~ "$thing"
	fi

	if [[ "$INSTALL" == true ]]; then
		stow -vt ~ "$thing"
	fi
done

if [[ "$INSTALL" == true ]]; then
	read -e -r -p "Enter git username: " username
	read -e -r -p "Enter git email: " email

	git config --global user.email "$email"
	git config --global user.name "$username"
fi
