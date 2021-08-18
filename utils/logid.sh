#!/bin/bash

set -euo pipefail

source ./sudo.sh

check_is_sudo

echo Setting up logitech mouse config...

cp ../config/logid.cfg /etc/logid.cfg

echo Done.