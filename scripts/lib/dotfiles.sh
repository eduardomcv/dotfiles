#!/bin/bash

dotfiles() {
	if ! command -v stow >/dev/null 2>&1; then
		echo "Error: Stow is required for installing dotfiles."
		exit 1
	fi

	# The stow directory is the same as the repository root
	STOW_DIR="$(git rev-parse --show-toplevel)"

	if [ $? -ne 0 ]; then
		echo "Error: Not inside a Git repository."
		exit 1
	fi

	# Store the original directory where we invoked the script
	ORIGINAL_DIR="$(pwd)"

	# Names of directories to stow (i.e. packages)
	PACKAGES=(
		git
		zsh
		neovim
		tmux
		ghostty
		wezterm
	)

	INSTALL=false
	UNINSTALL=false

	if [[ "$#" == 0 || "$1" == "install" ]]; then
		INSTALL=true
	fi

	if [[ "$#" == 0 || "$1" == "uninstall" ]]; then
		UNINSTALL=true
	fi

	# Change to stow directory
	cd "$STOW_DIR" || exit 1

	# Stow packages listed in PACKAGES
	for PACKAGE in ${PACKAGES[@]}; do
		if [[ "$UNINSTALL" == true ]]; then
			stow -vDt ~ "$PACKAGE"
		fi

		if [[ "$INSTALL" == true ]]; then
			stow -vt ~ "$PACKAGE"
		fi
	done

	if [[ "$INSTALL" == true ]]; then
		# Prompt the user for the git username and email
		read -e -r -p "Enter git username: " USERNAME
		read -e -r -p "Enter git email: " EMAIL

		git config --global user.email "$EMAIL"
		git config --global user.name "$USERNAME"
	fi

	# Revert to original directory
	cd "$ORIGINAL_DIR" || exit 1
}
