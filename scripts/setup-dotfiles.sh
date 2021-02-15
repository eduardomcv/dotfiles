#!/bin/bash

set -euo pipefail

source ./utils/sudo.sh

check_is_not_sudo

echo Synching...
rsync --quiet -avh --no-perms ../sync ~
echo Done.

