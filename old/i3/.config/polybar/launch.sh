#!/usr/bin/env bash

# Terminate already running bar instances
# If all your bars have ipc enabled, you can use 
polybar-msg cmd quit
# Otherwise you can use the nuclear option:
# killall -q polybar

# Find package coretemp
for i in /sys/class/hwmon/hwmon*/temp*_input; do 
    if [ "$(<$(dirname $i)/name): $(cat ${i%_*}_label 2>/dev/null || echo $(basename ${i%_*}))" = "coretemp: Package id 0" ]; then
        export HWMON_PATH="$i"
    fi
done

# Launch main bar
echo "---" | tee -a /tmp/polybar-main.log
# to add more bars, just copy the line below and change the bar name
# also add a new log file in /tmp
polybar main 2>&1 | tee -a /tmp/polybar-main.log & disown

echo "Bars launched..."
