#!/usr/bin/env bash
set -e

here="$( cd "$( dirname "$0" )" && pwd )"

# old dotfiles backup directory
current_time=$(date "+%Y_%m_%d-%H_%M_%S")
olddir_base="$here/dotfiles_old"
olddir="$olddir_base/$current_time"

# files to skip linking in the this directory
skipfiles="README.md `basename $0` . .. dotfiles_old"

################################################################################
## Functions
################################################################################

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
        echo "Backing up: $dot_file --> $olddir"
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

_trim_backups() {
    # trim old backups so we don't go crazy!

    local max_backups=6
    local backups=$(ls -1 "$olddir_base" | wc -l)

    # remove oldest until we are under max
    while [ $backups -ge $max_backups ]
    do
        oldest="$(ls -1 -t "$olddir_base" | tac | head -1)"
        _rm_backup "$oldest"
        backups=$(ls -1 "$olddir_base" | wc -l)
    done
}

_rm_backup() {
    # pretty safe rm rf

    local dir="$olddir_base/$1"
    echo "Removing backup: $dir"
    rm -rf "$dir"
}

################################################################################
## Main
################################################################################

# setup the bin
mkdir -p $HOME/bin

if [ $( grep -ic 'export PATH="\$PATH:$HOME/bin' ~/.bashrc ) -lt 1 ]
then
    echo 'export PATH="$PATH:~/bin"' >> ~/.bashrc
fi

# setup src and etc
mkdir -p $HOME/src
mkdir -p $HOME/src/etc

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

    echo "Linking files, any existing dotfiles backed up from ~ to $olddir"

    _create_dot_file "$file"
done

_setup_neovim
_trim_backups

# DONE
