## dotfiles revamp

Simple install script via [https://github.com/michaeljsmalley/dotfiles/blob/master/makesymlinks.sh](this example)

Different environments should be separate branches. Master should always be the _base_ files, branches should pull from there

#### Xdefaults include syntax

    !Based on solarized color scheme

    #include "/home/collin/dotfiles/urxvt/schemes/solarized"

#### PS1 stuff

    PS1='${debian_chroot:+($debian_chroot)}╭ \[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\n╰ \$ '

    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '

    ╭ ╰

