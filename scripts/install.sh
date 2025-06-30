#!/bin/bash

set -euo pipefail

# I'm assuming that, in order to use these scripts, git has been installed and this repository has been cloned.
# Then, all scripts and sources are located in reference to the root of this repo.
REPO_ROOT="$(git rev-parse --show-toplevel)"
# Store the original directory where we invoked the script
ORIGINAL_DIR="$(pwd)"

if [ -z "$REPO_ROOT" ]; then
	echo "Not in a git repository"
	exit 1
fi

# Change into the repository root directory
cd $REPO_ROOT || exit 1

source ./scripts/lib/os.sh

require_non_root

case "$(get_package_manager)" in
apt)
	source ./scripts/lib/apt.sh
	install_apt
	;;

pacman)
	source ./scripts/lib/pacman.sh
	install_pacman
	;;

brew)
	source ./scripts/lib/brew.sh
	check_brew
	install_brew
	;;

*)
	echo "Couldn't install dependencies."
	exit 1
	;;
esac

create_user_bin

source ./scripts/lib/dotfiles.sh
dotfiles install

# Return to the original directory
cd $ORIGINAL_DIR || exit 1
