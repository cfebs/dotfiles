#!/usr/bin/env bash
set -e

here="$( cd "$( dirname "$0" )" && pwd )"

# old dotfiles backup directory
current_time=$(date "+%Y_%m_%d-%H_%M_%S")
olddir="$here/dotfiles_old/$current_time"

# files to skip linking in the this directory
skipfiles="README.md `basename $0` . .. dotfiles_old"

# setup the bin
mkdir -p $HOME/bin
echo 'export PATH="$PATH:$HOME/bin"' >> $HOME/.bashrc

# setup src and etc
mkdir -p $HOME/src
mkdir -p $HOME/src/etc

# should_skip the linking/backup of file
# echo's 1 if it should be skipped
_should_skip() {
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

_create_dot_file() {
    # Backs up then links a dot file

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

_setup_neovim() {
    # Link .vim* -> .nvim*

    ln -sf ~/.vim ~/.nvim
    ln -sf ~/.vimrc ~/.nvimrc
}

################################################################################
## Main
################################################################################

# create dotfiles_old
echo "Creating $olddir for backup of any existing dotfiles in $olddir"
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

_setup_neovim

# DONE
