#!/bin/bash

set -euo pipefail

# I'm assuming that, in order to use these scripts, git has been installed and this repository has been cloned.
# Then, all scripts and sources are located in reference to the root of this repo.
REPO_ROOT="$(git rev-parse --show-toplevel)"

if [ $? -ne 0 ]; then
	echo "Error: Not inside a Git repository."
	exit 1
fi

source "$REPO_ROOT/scripts/lib/os.sh"

require_non_root

case "$(get_package_manager)" in
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

*)
	echo "Couldn't install dependencies."
	exit 1
	;;
esac

create_user_bin

source "$REPO_ROOT/scripts/lib/dotfiles.sh"
dotfiles install
