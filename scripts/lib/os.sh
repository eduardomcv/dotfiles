#!/usr/bin/env bash

function require_root() {
	if [[ "$(id -u)" != 0 ]]; then
		echo "Please run as root." >&2
		return 1
	fi
}

function require_non_root() {
	if [[ "$(id -u)" == 0 ]]; then
		echo "Please run as non-root." >&2
		return 1
	fi
}

function get_os_package_manager() {
	if command -v brew >/dev/null 2>&1 || [[ "$(uname -s)" == "Darwin" ]]; then
		echo "brew"
		return 0
	fi

	if command -v apt >/dev/null 2>&1; then
		echo "apt"
		return 0
	fi

	if command -v pacman >/dev/null 2>&1; then
		echo "pacman"
		return 0
	fi

	if command -v dnf >/dev/null 2>&1; then
		echo "dnf"
		return 0
	fi

	echo "Error: couldn't figure out the system package manager" >&2
	return 1
}

function create_user_bin() {
	mkdir -p ~/.local/bin
}
