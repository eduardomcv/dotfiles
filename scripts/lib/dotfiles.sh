#!/bin/bash

DOTFILES_STOW_TARGET="$HOME"

check_can_install_dotfiles() {
	if ! command -v stow >/dev/null 2>&1; then
		echo "Error: stow is required for managing dotfiles" >&2

		return 1
	fi

	git rev-parse --show-toplevel >/dev/null 2>&1

	if [ $? -ne 0 ]; then
		echo "Error: not inside a git repository" >&2
		return 1
	fi
}

install_dotfiles() {
	check_can_install_dotfiles

	if [[ "$?" != 0 ]]; then
		return 1
	fi

	# The stow directory is the same as the repository root
	local stow_dir="$(git rev-parse --show-toplevel)"

	# Store the original directory where we invoked the script
	local original_dir="$(pwd)"

	# Change to stow directory
	cd "$stow_dir" || return 1

	# Simulate stowing from args
	for config in "$@"; do
		stow -nt "$DOTFILES_STOW_TARGET" "$config" 2>/dev/null

		if [[ "$?" != 0 ]]; then
			echo "Error: the stow directory $stow_dir does not contain a config for $config"
			return 1
		fi
	done

	# Stow packages from args
	for config in "$@"; do
		stow -t "$DOTFILES_STOW_TARGET" "$config"
	done

	# Revert to original directory
	cd "$original_dir" || return 1

	echo "Installed dotfiles for $@!"
}

uninstall_dotfiles() {
	check_can_install_dotfiles

	# The stow directory is the same as the repository root
	stow_dir="$(git rev-parse --show-toplevel)"

	# Store the original directory where we invoked the script
	original_dir="$(pwd)"

	# Change to stow directory
	cd "$stow_dir" || return 1

	# Simulate un-stowing from args
	for config in "$@"; do
		stow -nDt "$DOTFILES_STOW_TARGET" "$config" 2>/dev/null

		if [[ "$?" != 0 ]]; then
			echo "Error: the stow directory $stow_dir does not contain a config for $config"
			return 1
		fi
	done

	for config in "$@"; do
		stow -Dt "$DOTFILES_STOW_TARGET" "$config"
	done

	# Revert to original directory
	cd "$original_dir" || return 1

	echo "Uninstalled dotfiles!"
}
