#!/usr/bin/env python3
import sys

if sys.platform == "darwin":
    print("listen_on unix:/tmp/mykitty")
else:
    # Other Linux: abstract socket
    print("listen_on unix:@mykitty")
