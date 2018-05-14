#!/usr/bin/env bash

export DISPLAY=:0 # for cron
mem="$(free -m)"

if echo "$mem" | grep -q 'available'
then
    left="$(free -m | grep '^Mem' | awk '{print $7}')"
else
    left="$(free -m | grep '^Mem' | awk '{print $4}')"
fi

#echo "MEM: $left" | logger -t notifyswapping
[ "$left" -lt "700" ] && notify-send "Mem left: $left"
