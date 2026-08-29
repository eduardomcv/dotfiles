#!/usr/bin/env python3
import os

theme = os.path.expanduser("~/.local/state/omarchy/current/theme/kitty.conf")
if os.path.exists(theme):
    print(f"include {theme}")
