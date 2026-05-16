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

function get_os_base() {
	if [[ "$(uname -s)" == "Darwin" ]]; then
		echo "macos"
		return 0
	fi

	if command -v apt >/dev/null 2>&1; then
		echo "debian"
		return 0
	fi

	if command -v pacman >/dev/null 2>&1; then
		echo "arch"
		return 0
	fi

	if command -v dnf >/dev/null 2>&1; then
		echo "fedora"
		return 0
	fi

	echo "Error: couldn't figure out the operating system base" >&2
	return 1
}

function create_user_bin() {
	mkdir -p ~/.local/bin
}
