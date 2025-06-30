#!/bin/bash

dotfiles() {
	INSTALL=false
	UNINSTALL=false

	THINGS_TO_STOW=(
		git
		zsh
		neovim
		tmux
		ghostty
	)

	if [[ "$#" == 0 || "$1" == "install" ]]; then
		INSTALL=true
	fi

	if [[ "$#" == 0 || "$1" == "uninstall" ]]; then
		UNINSTALL=true
	fi

	for THING in ${THINGS_TO_STOW[@]}; do
		if [[ "$UNINSTALL" == true ]]; then
			stow -vDt ~ "$THING"
		fi

		if [[ "$INSTALL" == true ]]; then
			stow -vt ~ "$THING"
		fi
	done

	if [[ "$INSTALL" == true ]]; then
		# Prompt the user for the git username and email
		read -e -r -p "Enter git username: " USERNAME
		read -e -r -p "Enter git email: " EMAIL

		git config --global user.email "$EMAIL"
		git config --global user.name "$USERNAME"
	fi
}
