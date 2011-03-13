#!/bin/bash
cd ${HOME}
ln -s configs/.bashrc 
ln -s configs/.Xdefaults
ln -s configs/.vim
ln -s configs/.vimrc
ln -s configs/.gitconfig
ln -s configs/.vimperatorrc
ln -s configs/.vimperator
ln -s configs/.screenrc
ln -s configs/.inputrc 
ln -s configs/.config/awesome .config/
ln -s configs/.config/vimprobable .config/
ln -s configs/.elinks/ .
ln -s configs/.gemrc .
ln -s configs/.irbrc .
ln -s configs/.ncmpcpp/ .
ln -s configs/.rtorrent.rc .
ln -s configs/.xinitrc .
ln -s configs/.config/ranger .config/
ln -s configs

mkdir .vim/bin
ln -s .vim/bundle/vim-ruby-debugger/vim/bin/ruby_debugger.rb .vim/bin/
ln -s .vim/bundle/vim-ruby-debugger/vim/plugin/ruby_debugger.vim .vim/plugin/
ln -s .vim/bundle/vim-ruby-debugger/vim/doc/ruby_debugger.txt .vim/doc/ruby_debugger.txt

# optional
#ln -s configs/.asoundrc .
#ln -s configs/.xbindkeysrc .
#ln -s configs/.xmodmap 
 
