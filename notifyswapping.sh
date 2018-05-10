#!/usr/bin/env bash

export DISPLAY=:0 # for cron
left="$(free -m | grep '^Mem' | awk '{print $4}')"
#echo "MEM: $left" | logger -t notifyswapping
[ "$left" -lt "700" ] && notify-send "Mem left: $left"
