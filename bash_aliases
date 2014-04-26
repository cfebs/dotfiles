#!/bin/bash

export EDITOR='vim'

alias tmux="TERM=screen-256color-bce tmux"

# Quick nav
alias 1="cd ../"
alias 2="cd ../../"
alias 3="cd ../../../"
alias 4="cd ../../../../"

alias bashrc=". ~/.bashrc"

grep_alias='grep'
if grep --color "a" <<<"a" &>/dev/null; then
    grep_alias="$grep_alias --color=auto"
fi
alias grep="$grep_alias"

alias less="less -R"

# xampp
alias lampp="sudo /opt/lampp/lampp"
if [ -d "$lampp" ]
then
  export LAMPP_HOME="/opt/lampp/lampp"
fi

alias c='clear'
alias bc='bc -l'
alias lh='ls --hide="*.pyc"'
alias l='ls'

alias vi=vim
alias v=vim
alias svi='sudo vi'
alias vis='vim "+set si"'
alias edit='vim'

alias simple_http="python -m SimpleHTTPServer 4331"
alias simp="$HOME/web/rb/simple-server/simple_server.ru -p 4331"

alias rm='rm'
# do not delete / or prompt if deleting more than 3 files at a time #
TEST_FILE="$HOME/.rm_preserve_test" && touch $TEST_FILE && rm --preserve-root $TEST_FILE 2> /dev/null && \
    alias rm='rm --preserve-root'

TEST_FILE="$HOME/.rm_preserve_test" && touch $TEST_FILE && rm -I $TEST_FILE 2> /dev/null && \
    alias rm='rm -I'

alias g="git"
__git_complete g __git_main

# confirmation #
alias mv='mv -i'
alias cp='cp -i'
alias ln='ln -i'

alias sag='sudo apt-get'

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

    [[ -z $1 ]] \
        && echo 'No search param' && return 0

    local q="$1"

    shift

    local dir="$1"

    [[ -z "$dir" ]] && dir="."

    find "$dir" -iname "*$q*"
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

alias htmlencode="perl -MHTML::Entities -pe 'encode_entities(\$_)'"
alias htmldecode="perl -MHTML::Entities -pe 'decode_entities(\$_)'"

function echo_error() {
    echo -n "$(red "(ERROR)") " 1>&2
}


function canonical_path()
{
    local path="$1" ; shift
    if [ -d "$path" ]
    then
        echo "$(cd "$path" ; pwd)"
    else
        local b=$(basename "$path")
        local p=$(dirname "$path")
        echo "$(cd "$p" ; pwd)/$b"
    fi
}

function easy_link()
{
    if [[ -z "$1" ]] || [[ "$1" = '--help' ]]; then
        echo "Usage: $FUNCNAME <link_target> <link_destination>" 1>&2
        cat 1>&2 <<HELP

    This is a shortcut for a simple ln -s operation.

    Relative paths and expansions are supported.

    TODO mysysgit support
HELP

        return 0;
    fi

    local quiet=true
    [[ "$1" = "--verbose" ]] && quiet=false && shift;

    local target="$1"
    local link_dest="$2"


    [[ -z "$target" ]] && echo "Target: '$target' was not provided" 1>&2 && return 1;
    [[ -z "$link_dest" ]] && echo "Link Destination: '$link_dest' was not provided" 1>&2 && return 1;

    [[ ! -e "$target" ]] && echo "Target: '$target' was not found, can't make link" 1>&2 && return 1;

    local cmd="readlink -f"
    type readlink >/dev/null 2>&1 || cmd="canonical_path"

    local target_path="$("$cmd" "$target")"
    local link_dest_path="$("$cmd" "$link_dest")"

    [[ -e $link_dest_path ]] && echo "Link Destination path '$link_dest_path' exists already" 1>&2 && return 1

    local dir_link_option=""
    [[ -d "$target_path" ]] && dir_link_option="/D "

    if type ls.exe > /dev/null 2>&1;then
        local dos_target_path="$(echo $target_path | sed 's/^\///' | sed 's/\//\\/g' | sed 's/^./\0:/')"
        local dos_link_dest_path="$(echo $link_dest_path | sed 's/^\///' | sed 's/\//\\/g' | sed 's/^./\0:/')"

        local mklink="mklink $dir_link_option$dos_link_dest_path $dos_target_path"
        if [[ "$quiet" = true ]]
        then
            echo $mklink | cmd /C > /dev/null
        else
            echo $mklink | cmd /C
        fi

    elif type ln > /dev/null 2>&1; then

        echo ln -s "$target_path" "$link_dest_path"
    fi

    ### Validate
    if [[ -e "$link_dest_path" ]]
    then
        echo "Sym link created at '$link_dest_path'"
    else
        echo "Link was not created"
    fi
}
