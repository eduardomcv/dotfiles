#!/bin/bash

set -euo pipefail

# This script should be used inside the dotfiles git repository.
# All scripts are sourced relative to the repository's root directory.
REPO_ROOT="$(git rev-parse --show-toplevel)"

source "$REPO_ROOT/scripts/lib/os.sh"
require_non_root

source "$REPO_ROOT/scripts/lib/dotfiles.sh"
install_dotfiles omarchy

HYPRLAND_CONFIG="$HOME/.config/hypr/hyprland.conf"
OVERRIDES_CONFIG="$HOME/.config/hypr/overrides.conf"
SOURCE_LINE="source = $OVERRIDES_CONFIG"

if [ ! -f "$HYPRLAND_CONFIG" ]; then
	echo "Error: hyprland config not found" >&2
	exit 1
fi

if grep -Fxq "$SOURCE_LINE" "$HYPRLAND_CONFIG"; then
	echo "Error: overrides have already been added" >&2
	exit 1
fi

echo "" >>"$HYPRLAND_CONFIG"
echo "$SOURCE_LINE" >>"$HYPRLAND_CONFIG"

echo "Omarchy overrides installed!"
