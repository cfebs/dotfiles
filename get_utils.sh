#!/usr/bin/env bash

# assumes ~/src/etc

# vim Plug
if [ -f ~/.vim/autoload/plug.vim ]
then
    echo "Looks like Plug is already installed"
else
    mkdir -p ~/.vim/autoload
    echo "Getting Plug"
    curl -fLo ~/.vim/autoload/plug.vim https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi


# z
if [ -d ~/src/etc/z ]
then
    echo "Looks like z is already installed"
else
    echo "Getting z"
    git clone https://github.com/rupa/z.git ~/src/etc/z
    echo 'source ~/src/etc/z/z.sh' >> $HOME/.bashrc
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
    curl https://raw.githubusercontent.com/creationix/nvm/v0.24.1/install.sh | bash
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

echo "Done with getting utils"
