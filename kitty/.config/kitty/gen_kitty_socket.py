#!/usr/bin/env python3
import os
import sys

# Detect omarchy
is_omarchy = os.path.isdir("/usr/share/omarchy")
xdg_runtime = os.environ.get("XDG_RUNTIME_DIR")

if is_omarchy and xdg_runtime:
    # Omarchy pattern: per-instance socket in XDG_RUNTIME_DIR
    print(f"listen_on unix:{xdg_runtime}/omarchy-kitty-{{kitty_pid}}")
elif sys.platform == "darwin":
    # macOS: fixed path in /tmp
    print("listen_on unix:/tmp/mykitty")
else:
    # Other Linux: abstract socket
    print("listen_on unix:@mykitty")
