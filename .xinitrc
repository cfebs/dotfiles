#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources

fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

# Setup debug file
_debug() {
    echo "$(date -I'seconds') - $@" >> /tmp/xinit_rc_debug
}

_debug 'Starting custom xinitrc stuff'
_debug "xinitrc args: $@"

# sets up other important vars for use by i3 programs
# ex. flatpak uses XDG_DATA_DIRS, i3-dmenu-desktop needs this
if [ -f /etc/profile ]; then
    _debug 'Sourcing profile'
    source /etc/profile
fi

export_gtk_stuff=1
if [ "$1" = "/usr/bin/startplasma-x11" ]; then
    export_gtk_stuff=0
fi

# for i3's terminal selection
export TERMINAL=alacritty

if [ "$export_gtk_stuff" -eq "1" ]; then
    # for qt apps to use gtk themes when launched via rofi/dmenu in i3
    export QT_QPA_PLATFORMTHEME=gtk2

    # so every app that launches will export current GTK_THEME which should enable dark mode
    # in certain apps like chrome
    if [ -s "$XDG_CONFIG_HOME/gtk-3.0/settings.ini" ]; then
        theme_name="$(cat "$XDG_CONFIG_HOME/gtk-3.0/settings.ini" | grep '^gtk-theme-name' | awk -F= '{print $2}')"
        if [ -n "$theme_name" ]; then
            _debug "Exporting theme $theme_name"
            export GTK_THEME="$theme_name"
        fi
    fi
fi

_debug "$(env)"
_debug "Done"

if [ -z "$DBUS_SESSION_BUS_ADDRESS" ]; then
    _debug ".xinitrc: No session bus detected, using dbus-launch"
    exec dbus-launch $@
else
    exec $@
fi
