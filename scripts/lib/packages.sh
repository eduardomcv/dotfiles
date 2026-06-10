#!/usr/bin/env bash

function brew_source() {
	if [[ -x /home/linuxbrew/.linuxbrew/bin/brew ]]; then
		eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
	elif [[ -x /opt/homebrew/bin/brew ]]; then
		eval "$(/opt/homebrew/bin/brew shellenv)"
	fi
}

function brew_check() {
	# Try to source brew
	brew_source

	# Install homebrew if brew command could not be found
	if ! command -v brew >/dev/null 2>&1; then
		/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
		# Source after install
		brew_source
	fi
}

function install_macos() {
	brew_check

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
		lazygit \
		pngpaste \
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
		kitty \
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
		"peterwu/iosevka"
	)

	echo "Checking if COPRs are enabled..."
	local enabled_coprs
	enabled_coprs=$(dnf copr list 2>/dev/null)

	for name in "${COPR_NAMES[@]}"; do
		if ! echo "$enabled_coprs" | grep -q "${name}"; then
			echo "Enabling COPR: $name"
			sudo dnf copr enable -y "$name"
		else
			echo "COPR already enabled: $name"
		fi
	done
}

function install_dnf() {
	# update system
	sudo dnf update -y

	check_rpmfusion
	enable_copr

	# Enable Terra repositories
	sudo dnf install -y --nogpgcheck --repofrompath 'terra,https://repos.fyralabs.com/terra$releasever' terra-release

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
		ripgrep \
		fd \
		bat \
		eza \
		fzf \
		zoxide \
		mise \
		iosevka-fonts \
		shfmt \
		shellcheck \
		lua-language-server \
		neovim \
		kitty \
		helium-browser-bin

	brew_check

	brew install \
		tlrc \
		lazygit \
		bash-language-server \
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
		thunderbird

	brew_check

	brew install \
		ripgrep \
		fd \
		bat \
		fzf \
		zoxide \
		eza \
		tlrc \
		lazygit \
		usage \
		mise \
		shfmt \
		shellcheck \
		bash-language-server \
		lua-language-server \
		stylua \
		tree-sitter-cli \
		neovim
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
	local os_base

	dotfiles_dir="$(git rev-parse --show-toplevel)"
	source "$dotfiles_dir/scripts/lib/os.sh"

	os_base="$(get_os_base)"

	case $os_base in
	arch)
		install_pacman
		;;
	debian)
		install_apt
		;;
	fedora)
		install_dnf
		;;
	macos)
		install_macos
		;;
	*)
		echo "No install script exists for $os_base" >&2
		exit 1
		;;
	esac
}
