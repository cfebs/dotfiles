#!/bin/bash

export EDITOR='vim'

alias tmux="TERM=screen-256color-bce tmux"

#echo "tmux -> tmux something" | awk '{split($0, a, " -> "); print a[1], a[2]}'

# Quick nav
alias 1="cd ../"
alias 2="cd ../../"
alias 3="cd ../../../"
alias 4="cd ../../../../"

alias bashrc=". ~/.bashrc"
alias grep="grep --color"

# xampp
alias lampp="sudo /opt/lampp/lampp"
if [ -d "$lampp" ]
then
  export LAMPP_HOME="/opt/lampp/lampp"
fi

# http://www.cyberciti.biz/tips/bash-aliases-mac-centos-linux-unix.html
alias c='clear'
alias bc='bc -l'
alias lh='ls --hide="*.pyc"'
alias l='ls'

alias vi=vim
alias svi='sudo vi'
alias vis='vim "+set si"'
alias edit='vim'

alias simple_http="python -m SimpleHTTPServer 4331"
alias simp="$HOME/web/rb/simple-server/simple_server.ru -p 4331"

# do not delete / or prompt if deleting more than 3 files at a time #
alias rm='rm -I --preserve-root'

# confirmation #
alias mv='mv -i'
alias cp='cp -i'
alias ln='ln -i'

## Functions

rvm-project() {
  if [ -z "$1" ]
  then
    echo Need name argument
  else
    mkdir "$1"
    rvm gemset create "$1"
    echo "rvm gemset use $1" > ./$1/.rvmrc
    echo '.rvmrc' >> ./$1/.gitignore
  fi
}

function timer_notify() {
    echo -e "\n\nBOOOM! Time to start."
    notify-send -u critical "`echo BOOOOOOM | figlet`"
}

function timer() {
    MIN=$1 && for i in $(seq $(($MIN*60)) -1 1); do echo -n "$i, "; sleep 1; done; timer_notify;
}

alias ack="ack-grep"
function qfind() {
    if [ -z $1 ]
    then
      echo "No search param"
      return 0
    fi

    find . -iname "*$1*"
}

function qgrep() {
    grep -irn "$1" .
}

function net-qual() {
    # 1st arg is the domain to make a HEAD request to
    while true; do echo 'start'; time curl --head "$1"; echo 'finish & sleeping'; sleep 10; done;
}

function run_once() {
    if [ ! -f ~/.run_once ]
    then
        git config --global user.name  "febs"
        git config --global user.email "coojoe89@gmail.com"
        git config --global color.ui true

        echo "Ran run_once() in bash_aliases" > ~/.run_once
    fi
}
run_once
