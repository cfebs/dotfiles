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
