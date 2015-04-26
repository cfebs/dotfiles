#!/usr/bin/env bash
set -e

here="$( cd "$( dirname "$0" )" && pwd )"
source "$here/scriptutils.sh"

# parse options
while true
do
  case $1 in
    -v) # use verbose mode
        VERBOSE=true
        shift
        ;;
    -*)
        echo "Unknown option: $1"
        exit 1
        ;;
    *)
        break
        ;;
  esac
done

log() {
    echo "$@"
    return 0
}

logv() {
    [ $VERBOSE ] && echo "$@"
    return 0
}

# old dotfiles backup directory
current_time=$(date "+%Y_%m_%d-%H_%M_%S")
olddir_base="$here/dotfiles_backup"
olddir="$olddir_base/$current_time"

# files to skip linking in the this directory
skipfiles="README.md `basename $0` . .."

################################################################################
## Functions
################################################################################

_should_skip() {
    # should_skip the linking/backup of file
    # DEPRECATED

    skip=false
    file="$1"
    filename="`basename $file`"

    # do not link directories
    [ -d "$file" ] && return 0

    for skipfile in $skipfiles; do
        if [ "$filename" = "$skipfile" ]
        then
            return 0
        fi
    done

    return 1
}

_create_dot_file() {
    # Backs up then links a dot file

    file="$1"
    filename="`basename $file`"
    dot_file="$HOME/.$filename"

    if [ -e "$dot_file" ]
    then
        # backup
        logv "Backing up: $dot_file --> $olddir"
        cp -f "$dot_file" "$olddir"

        # remove old
        rm -f "$dot_file"
    fi

    logv "Creating symlink to $file in home directory."
    ln -s "$here/files/$filename"  "$dot_file"
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
    logv "Removing backup: $dir"
    rm -rf "$dir"
}

################################################################################
## Main
################################################################################

# setup the bin
mkdir -p $HOME/bin

if ! _does_line_exist_in_file 'export PATH="$PATH:$HOME/bin"' "$HOME/.bashrc"
then
    echo 'export PATH="$PATH:$HOME/bin"' >> ~/.bashrc
fi

# setup src and etc
mkdir -p $HOME/src
mkdir -p $HOME/src/etc

# create dotfiles_old
log "Creating $olddir for backup of any existing dotfiles in $olddir"
mkdir -p $olddir

# change to the dotfiles directory
logv "Changing to the $here directory"
cd $here

log "Linking files, any existing dotfiles backed up from ~ to $olddir"

for file in $here/files/*; do
    _create_dot_file "$file"
done

_setup_neovim
_trim_backups

log "Done with setup"

# DONE
