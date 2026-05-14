#!/usr/bin/env bash
# Installs the logiops configuration for logitech devices.

set -euo pipefail

DOTFILES_DIR="$(git rev-parse --show-toplevel)"

LOGID_CONFIG="$DOTFILES_DIR/config/logid.cfg"
DESTINATION="/etc/logid.cfg"

sudo cp "$LOGID_CONFIG" "$DESTINATION"
