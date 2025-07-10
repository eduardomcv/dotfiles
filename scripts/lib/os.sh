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
	# In Bazzite, install has to be handled differently due to the read-only file system.
	# An easy way to detect this system is to check the presence of the "ujust" utility.
	if command -v ujust >/dev/null 2>&1; then
		echo "bazzite"
		exit 0
	fi

	if command -v brew >/dev/null 2>&1 && [[ "$(uname -s)" == "Darwin" ]]; then
		# we're on MacOS and brew is available
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
