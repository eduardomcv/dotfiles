#!/bin/bash

# Generic install script. Attempts to find the available package manager, install dependencies,
# and stow dotfiles to the user's home directory.

set -euo pipefail

# This script should be used inside the dotfiles git repository.
# All scripts are sourced relative to the repository's root directory.
REPO_ROOT="$(git rev-parse --show-toplevel)"

if [ $? -ne 0 ]; then
	echo "Not inside the dotfiles repository." >&2
	exit 1
fi

source "$REPO_ROOT/scripts/lib/os.sh"

require_non_root

PACKAGE_MANAGER="$(get_package_manager)"

case PACKAGE_MANAGER in
apt)
	source "$REPO_ROOT/scripts/lib/apt.sh"
	install_apt
	;;

pacman)
	source "$REPO_ROOT/scripts/lib/pacman.sh"
	install_pacman
	;;

brew)
	source "$REPO_ROOT/scripts/lib/brew.sh"
	check_brew
	install_brew
	;;

dnf)
	source "$REPO_ROOT/scripts/lib/dnf.sh"
	install_dnf
	;;

bazzite)
	source "$REPO_ROOT/scripts/lib/bazzite.sh"
	install_bazzite
	;;

*)
	echo "No install script exists for $PACKAGE_MANAGER" >&2
	exit 1
	;;
esac

# Create ~/.local/bin
create_user_bin

# Install dotfiles
source "$REPO_ROOT/scripts/lib/dotfiles.sh"
install_dotfiles \
	git \
	zsh \
	neovim \
	ghostty
