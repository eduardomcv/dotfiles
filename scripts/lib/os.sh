#!/bin/bash

require_root() {
	# Require root access
	if [[ "$(id -u)" != 0 ]]; then
		echo "Please run as root." >&2
		return 1
	fi
}

require_non_root() {
	# Require non-root user
	if [[ "$(id -u)" == 0 ]]; then
		echo "Please run as non-root." >&2
		return 1
	fi
}

get_package_manager() {
	# In Bazzite, install has to be handled differently due to the read-only file system.
	# An easy way to detect this system is to check the presence of the "ujust" utility.
	if command -v ujust >/dev/null 2>&1; then
		echo "bazzite"
		return 0
	fi

	if command -v brew >/dev/null 2>&1 && [[ "$(uname -s)" == "Darwin" ]]; then
		# we're on MacOS and brew is available
		echo "brew"
		return 0
	fi

	if command -v apt >/dev/null 2>&1; then
		# apt is available
		echo "apt"
		return 0
	fi

	if command -v pacman >/dev/null 2>&1; then
		# pacman is available
		echo "pacman"
		return 0
	fi

	if command -v dnf >/dev/null 2>&1; then
		# dnf is available
		echo "dnf"
		return 0
	fi

	echo "Error: couldn't figure out the system package manager" >&2
	return 1
}

create_user_bin() {
	# create directory for user binaries
	mkdir -p ~/.local/bin
}
