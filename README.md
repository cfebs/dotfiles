## dotfiles

Now uses a `Makefile`

### Notes

#### Xdefaults include syntax

```
!Based on solarized color scheme

#include "/home/collin/dotfiles/urxvt/schemes/solarized"
```

#### PS1 stuff

```
PS1='${debian_chroot:+($debian_chroot)}╭ \[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\n╰ \$ '

PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '

╭ ╰
```

#### I3 keyring .profile

For using i3 with a distro that already relies on gnome-keyring

```
# in .profile

# For i3 keyring support
# Some debug
echo "From .profile 0: $0 @: $@" >> /tmp/profile_log
env | grep 'DESKTOP_SESSION' >> /tmp/profile_log

if [ "$0" == "/usr/sbin/lightdm-session" ] && [ "${DESKTOP_SESSION}" == "i3" ]; then
    eval $(gnome-keyring-daemon --start)
    export SSH_AUTH_SOCK
fi
```
