#!/bin/bash

set -euo pipefail

source ./utils/sudo.sh

check_is_not_sudo

rsync --quiet -avh --no-perms ../sync ~

