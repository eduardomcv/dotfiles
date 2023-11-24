#!/bin/bash

maim -f png /tmp/lock.png
convert /tmp/lock.png -scale 10% -scale 1000% /tmp/lock.png
[[ -f $1 ]] && convert /tmp/lock.png $1 -gravity center -composite -matte /tmp/lock.png
i3lock -i /tmp/lock.png
rm /tmp/lock.png
