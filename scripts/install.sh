#!/usr/bin/env bash
# Generic install script. Attempts to find the available package manager,
# install dependencies, and stow dotfiles to the user's home directory.
# This script should be called inside the dotfiles git repository,
# since all scripts are sourced relative to the dotfiles's root directory.

set -euo pipefail

DOTFILES_DIR="$(git rev-parse --show-toplevel)"

source "$DOTFILES_DIR/scripts/lib/os.sh"
require_non_root

create_user_bin

source "$DOTFILES_DIR/scripts/lib/packages.sh"
install_packages

source "$DOTFILES_DIR/scripts/lib/dotfiles.sh"
install_dotfiles \
	git \
	zsh \
	mise \
	vim \
	neovim \
	ghostty

source "$DOTFILES_DIR/scripts/lib/git.sh"
set_user_git_info
