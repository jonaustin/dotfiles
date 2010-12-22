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
ln -s /home/jon/configs/.config/awesome /home/jon/.config/awesome
ln -s /home/jon/configs/.config/vimprobable /home/jon/.config/vimprobable
ln -s configs/.elinks/ .
ln -s configs/.gemrc .
ln -s configs/.irbrc .
ln -s configs/.ncmpcpp/ .
ln -s configs/.rtorrent.rc .
ln -s configs/.xinitrc .

mkdir .vim/bin
ln -s ${HOME}/.vim/bundle/vim-ruby-debugger/vim/bin/ruby_debugger.rb .vim/bin/
ln -s ${HOME}/.vim/bundle/vim-ruby-debugger/vim/plugin/ruby_debugger.vim .vim/plugin/
ln -s ${HOME}/.vim/bundle/vim-ruby-debugger/vim/doc/ruby_debugger.txt .vim/doc/ruby_debugger.txt

# optional
#ln -s configs/.asoundrc .
#ln -s configs/.inputrc .
#ln -s configs/.xbindkeysrc .
#ln -s configs/.xmodmap 

