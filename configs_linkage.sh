#!/bin/bash
ln -s ${HOME}/configs/.bashrc 
ln -s ${HOME}/configs/.Xdefaults
ln -s ${HOME}/configs/.vim
ln -s ${HOME}/configs/.vimrc
ln -s ${HOME}/configs/.gitconfig
ln -s ${HOME}/configs/.vimperatorrc
ln -s ${HOME}/configs/.vimperator
ln -s ${HOME}/configs/.screenrc
ln -s ${HOME}/configs/.inputrc 
ln -s ${HOME}/configs/.config/awesome ${HOME}/.config/
ln -s ${HOME}/configs/.config/vimprobable ${HOME}/.config/
ln -s ${HOME}/configs/.elinks/ .
ln -s ${HOME}/configs/.gemrc .
ln -s ${HOME}/configs/.irbrc .
ln -s ${HOME}/configs/.ncmpcpp/ .
ln -s ${HOME}/configs/.rtorrent.rc .
ln -s ${HOME}/configs/.xinitrc .
ln -s ${HOME}/configs/.config/ranger .config/
ln -s ${HOME}/configs

mkdir .vim/bin
ln -s ${HOME}/.vim/bundle/vim-ruby-debugger/vim/bin/ruby_debugger.rb .vim/bin/
ln -s ${HOME}/.vim/bundle/vim-ruby-debugger/vim/plugin/ruby_debugger.vim .vim/plugin/
ln -s ${HOME}/.vim/bundle/vim-ruby-debugger/vim/doc/ruby_debugger.txt .vim/doc/ruby_debugger.txt

# optional
#ln -s ${HOME}/configs/.asoundrc .
#ln -s ${HOME}/configs/.inputrc .
#ln -s ${HOME}/configs/.xbindkeysrc .
#ln -s ${HOME}/configs/.xmodmap 
 
