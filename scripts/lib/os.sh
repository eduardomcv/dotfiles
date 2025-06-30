#!/bin/bash

require_root() {
	# Require root access
	if [[ "$(id -u)" != 0 ]]; then
		echo "Please run as root."
		exit 1
	fi
}

require_non_root() {
	# Require non-root user
	if [[ "$(id -u)" == 0 ]]; then
		echo "Please run as non-root."
		exit 1
	fi
}

get_package_manager() {
	if command -v brew >/dev/null 2>&1 || [[ "$(uname -s)" == "Darwin" ]]; then
		# either brew is available, or we're on MacOS, so use brew
		echo "brew"
		exit 0
	fi

	if command -v apt >/dev/null 2>&1; then
		# apt is available
		echo "apt"
		exit 0
	fi

	if command -v pacman >/dev/null 2>&1; then
		# pacman is available
		echo "pacman"
		exit 0
	fi

	if command -v dnf >/dev/null 2>&1; then
		# dnf is available
		echo "dnf"
		exit 0
	fi

	# couldn't find the package manager
	echo "unknown"
	exit 1
}

create_user_bin() {
	# create directory for user binaries
	mkdir -p ~/.local/bin
}
