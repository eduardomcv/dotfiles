#!/usr/bin/env bash

function check_brew() {
	# Install homebrew if brew command does not exist
	if ! command -v brew >/dev/null 2>&1; then
		/bin/bash -c \
			"$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
	fi
}

function install_brew() {
	brew install \
		make \
		openssl@3 \
		readline \
		libyaml \
		gmp \
		autoconf \
		cmake \
		stow \
		ripgrep \
		fd \
		bat \
		fzf \
		zoxide \
		eza \
		tlrc \
		pngpaste \
		lazygit \
		usage \
		mise \
		tree-sitter-cli \
		shfmt \
		shellcheck \
		bash-language-server \
		lua-language-server \
		stylua \
		neovim

	brew install --cask \
		font-iosevka \
		ghostty \
		thunderbird \
		zen
}

function check_rpmfusion() {
	echo "Checking if RPM Fusion is enabled..."

	if ! dnf repolist | grep -q rpmfusion-nonfree; then
		sudo dnf config-manager setopt fedora-cisco-openh264.enabled=1

		sudo dnf install -y \
			"https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm" \
			"https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm"
	else
		echo "RPM Fusion is already enabled!"
	fi
}

function enable_copr() {
	local COPR_NAMES=(
		"atim/lazygit"
		"alternateved/eza"
		"jdxcode/mise"
		"scottames/ghostty"
		"peterwu/iosevka"
		"agriffis/neovim-nightly"
		"yorickpeterse/stylua"
	)

	echo "Checking if COPRs are enabled..."
	dnf copr list | grep -q "${COPR_NAMES[-1]}"

	if [[ $? == 1 ]]; then
		for name in "${COPR_NAMES[@]}"; do
			sudo dnf copr enable -y "$name"
		done
	else
		echo "COPRs are already enabled!"
	fi
}

function install_dnf() {
	# update system
	sudo dnf update -y &&
		check_rpmfusion &&
		enable_copr &&
		sudo dnf install -y \
			@development-tools \
			autoconf \
			make \
			bzip2 \
			openssl-devel \
			libyaml-devel \
			libffi-devel \
			readline-devel \
			gdbm-devel \
			ncurses-devel \
			perl-FindBin \
			zlib-ng-compat-devel \
			cmake \
			gcc-c++ \
			libtool \
			libvterm-devel \
			wl-clipboard \
			zsh \
			stow \
			fd-find \
			ripgrep \
			bat \
			fzf \
			tldr \
			zoxide \
			mise \
			lazygit \
			eza \
			ghostty \
			iosevka-fonts \
			neovim \
			shellcheck \
			nodejs-bash-language-server \
			stylua
}

function install_apt() {
	sudo apt update && sudo apt upgrade -y

	sudo apt install -y \
		build-essential \
		software-properties-common \
		git \
		stow \
		zsh \
		firefox \
		thunderbird
}

function install_pacman() {
	sudo pacman -Syu --noconfirm

	sudo pacman -S --noconfirm --needed \
		base-devel \
		cmake \
		libffi \
		libyaml \
		openssl \
		zip \
		unzip \
		git \
		stow \
		zsh \
		fd \
		ripgrep \
		wl-clipboard \
		fzf \
		zoxide \
		bat \
		eza \
		usage \
		mise \
		tealdeer \
		lazygit \
		shfmt \
		shellcheck \
		bash-language-server \
		lua-language-server \
		stylua \
		tree-sitter-cli \
		neovim
}

function install_packages() {
	local dotfiles_dir
	local package_manager

	dotfiles_dir="$(git rev-parse --show-toplevel)"
	source "$dotfiles_dir/scripts/lib/os.sh"

	package_manager="$(get_os_package_manager)"

	case $package_manager in
	apt)
		install_apt
		;;

	pacman)
		install_pacman
		;;

	brew)
		check_brew
		install_brew
		;;

	dnf)
		install_dnf
		;;

	*)
		echo "No install script exists for $package_manager" >&2
		exit 1
		;;
	esac
}
