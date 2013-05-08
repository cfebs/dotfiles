#!/usr/bin/env bash

set -e

echo '1'

here="`dirname $(readlink -f $0)`"

# old dotfiles backup directory
olddir="$HOME/dotfiles_old"             
skipfiles="README.md `basename $0` . .."     # files to skip linking
echo '2'

# create dotfiles_old in homedir
echo "Creating $olddir for backup of any existing dotfiles in ~"
mkdir -p $olddir
echo "...done"

# change to the dotfiles directory
echo "Changing to the $here directory"
cd $here
echo "...done"

# _should_skip file
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

function _clone_vundle() {
    vundle_dir="$HOME/.vim/bundle/vundle"

    if [ -d "$vundle_dir/.git" ] 
    then
        return
    fi

    echo "Cloning vundle to $vundle_dir"
    git clone https://github.com/gmarik/vundle.git "$vundle_dir"
}

## Main

files=$here/*
for file in $files; do

    if [ "`_should_skip $file`" -eq "1" ]
    then
        continue
    fi

    echo "Moving any existing dotfiles from ~ to $olddir"

    _create_dot_file "$file"
    _clone_vundle
done
