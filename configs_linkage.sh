#!/bin/bash
ln -s configs/.bashrc 
ln -s configs/.Xdefaults
ln -s configs/.vim
ln -s configs/.vimrc
ln -s configs/.gitconfig
ln -s configs/.vimperatorrc
ln -s configs/.vimperator
ln -s configs/.screenrc
ln -s configs/.inputrc 
ln -s configs/.config/awesome .config/awesome
ln -s configs/.config/vimprobable .config/vimprobable
mkdir .vim/bin
ln -s ${HOME}/.vim/bundle/vim-ruby-debugger/vim/bin/ruby_debugger.rb .vim/bin/
ln -s ${HOME}/.vim/bundle/vim-ruby-debugger/vim/plugin/ruby_debugger.vim .vim/plugin/
ln -s ${HOME}/.vim/bundle/vim-ruby-debugger/vim/doc/ruby_debugger.txt .vim/doc/ruby_debugger.txt

