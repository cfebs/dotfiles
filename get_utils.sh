#!/usr/bin/env bash

HERE="$( cd "$( dirname "$0" )" && pwd )"
source "$HERE/scriptutils.sh"

# assumes ~/src/etc

# vim Plug
if [ -f ~/.vim/autoload/plug.vim ]
then
    echo "Looks like Plug is already installed"
else
    mkdir -p ~/.vim/autoload
    echo "Getting Plug"
    wget -O ~/.vim/autoload/plug.vim https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi


# z
z_source='source ~/src/etc/z/z.sh'
if [ -d ~/src/etc/z ]
then
    echo "Looks like z is already installed"
else
    echo "Getting z"
    git clone https://github.com/rupa/z.git ~/src/etc/z
fi

if ! _does_line_exist_in_file "$z_source" "$HOME/.bashrc"
then
    echo "$z_source" >> $HOME/.bashrc
fi


# fzf
if [ -d ~/.fzf ]
then
    echo "Looks like fzf is already installed"
else
    echo "Getting fzf"
    git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
    ~/.fzf/install
fi

# nvm
if [ -d ~/.nvm ]
then
    echo "Looks like nvm is already installed"
else
    echo "Getting nvm"
    git clone https://github.com/creationix/nvm.git ~/.nvm && cd ~/.nvm && git checkout `git describe --abbrev=0 --tags`
fi

nvm_source='source ~/.nvm/nvm.sh'
if ! _does_line_exist_in_file "$nvm_source" "$HOME/.bashrc"
then
    echo "$nvm_source" >> $HOME/.bashrc
fi


# rbenv
if [ -d ~/.rbenv ]
then
    echo "Looks like rbenv is already installed"
else
    echo "Getting rbenv"
    git clone https://github.com/sstephenson/rbenv.git ~/.rbenv
    git clone https://github.com/sstephenson/ruby-build.git ~/.rbenv/plugins/ruby-build
fi

# rake completion
if [ -d ~/src/etc/rake-completion ]
then
    echo "Looks like rake completion is already installed"
else
    git clone https://github.com/ai/rake-completion.git ~/src/etc/rake-completion
    echo 'source ~/src/etc/rake-completion/rake' >> $HOME/.bashrc
fi
rake_source='source ~/src/etc/rake-completion/rake'
if ! _does_line_exist_in_file "$rake" "$HOME/.bashrc"
then
    echo "$rake_source" >> $HOME/.bashrc
fi

# git completion and prompt
if [ -f ~/src/etc/git-completion.bash ]
then
    echo "Looks like git completion is already installed"
else
    wget https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash -O ~/src/etc/git-completion.bash
fi

# git completion 2.17 and prompt
if [ -f ~/src/etc/git-completion217.bash ]
then
    echo "Looks like git completion 217 is already installed"
else
    wget https://raw.githubusercontent.com/git/git/v2.17.1/contrib/completion/git-completion.bash -O ~/src/etc/git-completion217.bash
fi

echo "Done with getting utils"
