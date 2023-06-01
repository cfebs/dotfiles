#!/bin/sh

if [ -n "$WAYLAND_DISPLAY" ]; then
	echo "Not running xrandr, looks like we are in wayland because WAYLAND_DISPLAY is set"
	exit 0
fi
xrandr --output DisplayPort-0 --primary --mode 2560x1440 --pos 0x0 --rotate normal --output DisplayPort-1 --mode 2560x1440 --pos 2560x0 --rotate left --output DisplayPort-2 --off --output HDMI-A-0 --off
