#!/usr/bin/env bash
set -e

here="$( cd "$( dirname "$0" )" && pwd )" 

# old dotfiles backup directory
olddir="$HOME/dotfiles_old"

# files to skip linking in the this directory
skipfiles="README.md `basename $0` . .."

# setup the bin
mkdir -p $HOME/.bin
echo 'export PATH="$PATH:$HOME/.bin"' >> $HOME/.bashrc

# setup src
mkdir -p $HOME/src

# _should_skip the linking/backup of file
# echo's 1 if it should be skipped
function _should_skip() {

    skip=0
    file="$1"
    filename="`basename $file`"

    for skipfile in $skipfiles; do
        if [ "$filename" = "$skipfile" ]
        then
            skip=1
        fi
    done

    # do not link directories
    if [ -d "$file" ]
    then
        skip=1
    fi

    echo "$skip"

}

function _create_dot_file() {

    file="$1"
    filename="`basename $file`"
    dot_file="$HOME/.$filename"


    if [ -e "$dot_file" ]
    then
        # backup
        cp -f "$dot_file" "$olddir"

        # remove old
        rm -f "$dot_file"
    fi

    echo "Creating symlink to $file in home directory."
    ln -s "$here/$filename"  "$dot_file"

}


################################################################################
# DEPRECATED, using microbe now
################################################################################
function _clone_vundle() {
    vundle_dir="$HOME/.vim/bundle/vundle"

    if [ -d "$vundle_dir/.git" ]
    then
        return
    fi

    mkdir -p $vundle_dir
    echo "Cloning vundle to $vundle_dir"
    git clone https://github.com/gmarik/vundle.git "$vundle_dir"
}

function _clone_microbe() {
    local reop="microbe-vim"
    if [ ! -d "$HOME/src/$repo" ]
    then
        git clone https://github.com/xsc/$repo.git "$HOME/src/$repo"
    	return 0
    fi

    ln -s "$HOME/src/$repo/bin/microbe" "$HOME/.bin/microbe"
    chmod +x ~/.bin/microbe
}

## Main

# create dotfiles_old in homedir
echo "Creating $olddir for backup of any existing dotfiles in ~"
mkdir -p $olddir

# change to the dotfiles directory
echo "Changing to the $here directory"
cd $here

files=$here/*
for file in $files; do

    if [ "`_should_skip $file`" -eq "1" ]
    then
        continue
    fi

    echo "Moving any existing dotfiles from ~ to $olddir"

    _create_dot_file "$file"
done

_clone_microbe
