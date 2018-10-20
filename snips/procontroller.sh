#!/usr/bin/env bash

# Steps
# Connect it to your pc via bluetooth. I use blueman and Gnome Bluetooth. LEDs still blink left-right even when connected.
# Find your device id via “sudo evtest”
# Method 1: Calibrate the evdev device
#
# Configure the calibration as follows, changing from eventX to your device’s id:
# # We're defining a temporary variable only for convenivence
# device=/dev/input/eventX
#
# # Left stick X, Y then right stick X, Y. You can copy/paste this whole block to your terminal
# sudo evdev-joystick --e $device --a 0 --minimum 5000 --maximum 52000 & \
# sudo evdev-joystick --e $device --a 1 --minimum 10000 --maximum 60000 & \
# sudo evdev-joystick --e $device --a 3 --minimum 8000 --maximum 52000 & \
# sudo evdev-joystick --e $device --a 4 --minimum 10000 --maximum 60000

# You can check the current calibration with this command
# sudo evdev-joystick --s $device

devicenum="$(cat /proc/bus/input/devices | grep -A5 "Pro Controller" | grep Handlers= | awk -F'=' '{print $2}' | awk '{print $1}')"

if [ -z "$devicenum" ]; then
    echo "Couldn't find a device yikes!"
    exit 1
fi


device="/dev/input/${devicenum}"
echo "Found $device" 1>&2

# Left stick X, Y then right stick X, Y. You can copy/paste this whole block to your terminal
sudo evdev-joystick --e $device --a 0 --minimum 5000 --maximum 52000 & \
sudo evdev-joystick --e $device --a 1 --minimum 10000 --maximum 60000 & \
sudo evdev-joystick --e $device --a 3 --minimum 8000 --maximum 52000 & \
sudo evdev-joystick --e $device --a 4 --minimum 10000 --maximum 60000

# You can check the current calibration with this command
sudo evdev-joystick --s $device
