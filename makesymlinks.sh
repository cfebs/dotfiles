#!/bin/bash
############################
# .make.sh
# This script creates symlinks from the home directory to any desired dotfiles in ~/dotfiles
############################

########## Variables

dir=~/dotfiles                    # dotfiles directory
olddir=~/dotfiles_old             # old dotfiles backup directory
skipfiles="README.md `basename $0` . .."     # files to skip linking
curdir="`pwd`"


##########

# create dotfiles_old in homedir
echo "Creating $olddir for backup of any existing dotfiles in ~"
mkdir -p $olddir
echo "...done"

# change to the dotfiles directory
echo "Changing to the $dir directory"
cd $dir
echo "...done"

# move any existing dotfiles in homedir to dotfiles_old directory
# create symlinks from the homedir to any files in the ~/dotfiles directory specified in $files
files=$curdir/*
for file in $files; do
    skip=0
    filename="`basename $file`"

    for skipfile in $skipfiles; do
        if [ "$filename" = "$skipfile" ]
        then
            skip=1
        fi
    done

    # do not link directories
    if [ -d $file ]
    then
        skip=1
    fi

    if [ "$skip" -eq "1" ]
    then
        continue
    fi

    echo "Moving any existing dotfiles from ~ to $olddir"

    if [ -f  ~/.$filename ]
    then
        mv ~/.$filename ~/dotfiles_old/
    fi

    echo "Creating symlink to $file in home directory."

    ln -s $dir/$filename ~/.$filename
done

# back to where we started
cd "$curdir"

echo "Cloning vundle to ~/.vim/bundle/vundle"

git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
