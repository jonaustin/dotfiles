" Modeline and Notes {
" vim: set sw=2 ts=2 sts=2 et tw=78 foldmarker={,} foldlevel=0 foldmethod=marker ft=vim
"
" }

" vim-plug {{{
  " Specify a directory for plugins
  " - For Neovim: ~/.local/share/nvim/plugged

  " Auto-install
  if empty(glob('~/.vim/autoload/plug.vim'))
    silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
          \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
  endif

  call plug#begin('~/.vim/bundle')

  " deoplete
  "if has('nvim')
  "  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
  "else
  "  Plug 'Shougo/deoplete.nvim'
  "  Plug 'roxma/nvim-yarp'
  "  Plug 'roxma/vim-hug-neovim-rpc'
  "endif
  "Plug 'Shougo/neco-syntax'
  "Plug 'zchee/deoplete-zsh'
  "Plug 'zchee/deoplete-jedi', { 'do': 'pip install jedi' } " for use with jedi-vim: https://github.com/zchee/deoplete-jedi/issues/35#issuecomment-281791696
  "Plug 'carlitux/deoplete-ternjs', { 'do': 'npm install -g tern' }
  "Plug 'ternjs/tern_for_vim'
  "Plug 'Shougo/deoplete-rct'

  Plug 'roxma/nvim-completion-manager'
  Plug 'roxma/nvim-cm-tern',  {'do': 'npm install'}
  "Plug 'ajh17/VimCompletesMe'
  "Plug 'ternjs/tern_for_vim'

  " Ruby
  "Plug 'ecomba/vim-ruby-refactoring'
  Plug 'tpope/vim-haml'
  Plug 'vim-ruby/vim-ruby'

  " Javascript
  "Plug 'pangloss/vim-javascript'
  "Plug 'jelera/vim-javascript-syntax'
  "Plug 'othree/yajs.vim'
  "Plug 'vim-scripts/JavaScript-Indent'
  "Plug 'othree/javascript-libraries-syntax.vim'
  "Plug 'kchmck/vim-coffee-script'
  "Plug 'mmalecki/vim-node.js' " detect node shebang and set FT to JS
  ""Plug 'mtscout6/vim-cjsx' " coffeescript with react jsx
  "Plug 'mxw/vim-jsx'
  "Plug 'othree/yajs.vim' " yet another javascript syntax

  " typescript
  "Plug 'leafgarland/typescript-vim' " syntax
  "Plug 'Quramy/vim-js-pretty-template' " template strings coloring
  "Plug 'jason0x43/vim-js-indent'
  "Plug 'HerringtonDarkholme/yats.vim' " yet another typescript syntax
  "Plug 'Quramy/tsuquyomi' " tsserver
  "Plug 'mhartington/nvim-typescript'

  " Navigation
  "Plug 'kien/ctrlp.vim'
  "Plug 'FelikZ/ctrlp-py-matcher'        " Exact filename matches!
  "Plug 'goldfeld/vim-seek'              " <leader>s<2 chars>
  "Plug 'mileszs/ack.vim'                " :Ack <search>
  "Plug 'Lokaltog/vim-easymotion'        " <leader><leader>w
  "Plug 'jeetsukumaran/vim-buffergator'  " <leader>b
  "Plug 't9md/vim-choosewin'             " -

  " Integrations
  Plug 'skywind3000/asyncrun.vim' " used by other plugins to run things asynchronously (or :AsyncRun) Note: not compatible with vim-dispatch as it overrides :make
  Plug 'janko-m/vim-test'
  Plug 'metakirby5/codi.vim' " amazing repl
  Plug 'rizzatti/dash.vim' " Dash.app integration - :<leader>d / :Dash (word under cursor), :Dash printf, :Dash setTimeout javascript, :DashKeywords backbone underscore javascript
  "Plug 'scrooloose/nerdtree'
  Plug 'tpope/vim-fugitive'
  Plug 'tpope/vim-rhubarb' " Gbrowse for fugitive
  Plug 'tpope/vim-git'
  Plug 'tpope/vim-eunuch' " :Mkdir, :SudoWrite, :Chmod, etc
  Plug 'mattn/gist-vim' "github gists
  Plug 'tmux-plugins/vim-tmux'

  Plug 'tpope/vim-repeat'
  Plug 'tpope/vim-surround'

  " Colors
  Plug 'w0ng/vim-hybrid'

  " UI
  Plug 'vim-airline/vim-airline'
  Plug 'vim-airline/vim-airline-themes'

  " Misc
  Plug 'johngrib/vim-game-code-break'
  Plug 'wakatime/vim-wakatime'
  Plug 'AD7six/vim-activity-log'


  " Initialize plugin system
  call plug#end()
" }}}

" Basics {
  filetype plugin indent on  	        " Automatically detect file types.
	let mapleader = ","

  if has('unix')
    if has('mac')       " osx
      set clipboard=unnamed              " * register -- SYSTEM (middle-click) clipboard (with --version +xterm_clipboard)
    else " linux, bsd, etc
      set clipboard=unnamedplus         " >=7.3.74 only -- + register -- X11 (ctrl-c/v) clipboard
    endif
  endif

  set noautochdir                     " do not automatically change directory
  "set cryptmethod=blowfish            " strong blowfish encryption (instead of zip)
" }

" General {
	syntax on 					                " syntax highlighting
	set mouse=a
  set signcolumn=yes                " Otherwise realtime linter gets annoying
	" not every vim is compiled with this, use the following line instead
     "autocmd BufEnter * if bufname("") !~ "^\[A-Za-z0-9\]*://" | lcd %:p:h | endif
	scriptencoding utf-8
  set encoding=utf-8                 " no junk chars
	set autowrite
	set shortmess+=filmnrxoOtT          " abbrev. of messages (avoids 'hit enter')
  "set foldmethod=syntax
  set foldlevelstart=99

  " Speed
  set lazyredraw                      " fix horrible slowdown issues when moving cursor with syntax on
  set ttyfast                         " assume fast connection (smoother redraw)
  set synmaxcol=1024                  " Syntax coloring lines that are too long just slows down the world
  set nolist                          " Hide invisibles
  " Use the old regex engine
  " http://stackoverflow.com/questions/16902317/vim-slow-with-ruby-syntax-highlighting
  set re=1

  " Disable Ex mode
  map Q <Nop>

	" Setting up the directories {
  set backup 						            " backups are nice ...
  set backupdir=$HOME/.vimbackup    " but not when they clog .
  set directory=$HOME/.vimswap 	    " Same for swap files
  set viewdir=$HOME/.vimviews 	    " same but for view files
  set undodir=$HOME/.vimundo

  " Creating directories if they don't exist
  silent execute '!mkdir -p $HOME/.vimbackup'
  silent execute '!mkdir -p $HOME/.vimswap'
  silent execute '!mkdir -p $HOME/.vimviews'
  silent execute '!mkdir -p $HOME/.vimundo'

" Vim UI {
	set background=dark                 " Assume a dark background
  colo hybrid
  if has('unix')
    if has('mac')       " osx
    else " linux, bsd, etc
      hi Normal ctermbg=none
      hi NonText ctermbg=none
      hi LineNr ctermbg=none
      hi clear CursorLineNr
    endif
  endif
	set incsearch 					           " find as you type search
	set hlsearch 					             " highlight search terms
	set winminheight=0 				         " windows can be 0 line high
	set ignorecase 					           " case insensitive search
	set smartcase 					           " become temporarily case sensitive when any uppercase letters present in search string
  set undofile                       " undo even after closing and re-opening a file!

" Formatting {
	set wrap                      	   " wrap long lines
  set showbreak=↪                    " prettier line wrap
	set autoindent                 	   " indent at the same level of the previous line
	set shiftwidth=2               	   " use indents of 2 spaces

" Key Mappings {

  " Aliases {
  :command! Wq wq
  :command! WQ wq
  :command! W w
  :command! Q q
  :command! Qa qa
  "}

  " Windows

	" Easier moving in tabs and windows
	map <C-J> <C-W>j<C-W>_
  " resize horizontal split windows
  map <leader>j <C-W>10+
	map <leader>k <C-W>10-
  " resize vertical split window
  nmap <leader>h <C-W>10>
  nmap <leader>l <C-W>10<

  " move split window context
	map <C-K> <C-W>k<C-W>_
	map <C-L> <C-W>l<C-W>_
	map <C-H> <C-W>h<C-W>_
	map <C-K> <C-W>k<C-W>_
  " easy tab switching
	map <S-H> gT
	map <S-L> gt

	" Yank from the cursor to the end of the line, to be consistent with C and D.
	nnoremap Y y$
  " yank all lines
  nmap <leader>ya :%y+<cr>

	" remap jj to escape
	inoremap jj <ESC>
	inoremap jk <ESC>

  " buffers
    " buffer next
  :noremap <leader>bn :bn<cr>
    " buffer prev
  :noremap <leader>bp :bp<cr>

  " quickfix
  " open
  map <leader>qf :cope<cr>
  " close
  map <leader>cf :ccl<cr>

  " remove search highlights
  map <silent><leader>. :nohl<cr>

  " remove trailing whitespace
  nnoremap <leader>W :%s/\s\+$//<cr>:let @/=''<CR>

  " Airline/Powerline
  let g:airline_powerline_fonts = 1
  let g:airline_theme='base16'
  let g:airline#extensions#tabline#enabled = 1

  " vim-test
  let g:test#javascript#jasmine#file_pattern = '\v.*/.*spec\.(js|jsx|coffee)$'
  let test#strategy = 'neovim' "'neoterm'
  nmap <silent> <leader>t :TestNearest<CR>
  nmap <silent> <leader>T :TestFile<CR>
  "nmap <silent> <leader>a :TestSuite<CR>
  nmap <silent> <leader>l :TestLast<CR>
  nmap <silent> <leader>g :TestVisit<CR>


  " Deoplete
  let g:deoplete#enable_at_startup = 1
  let g:deoplete#auto_complete_delay = 0
  let g:python3_host_prog = '/home/jon/.pyenv/shims/python3'
  let g:python2_host_prog = '/home/jon/.pyenv/shims/python2'
  "let deoplete#tag#cache_limit_size = 5000000 " 5MB (for big ctags)
  let g:deoplete#sources = {}
  let g:deoplete#sources['javascript.jsx'] = ['ternjs'] "['file', 'ultisnips', 'ternjs']
  let g:deoplete#ignore_sources = {}
  let g:deoplete#ignore_sources['javascript.jsx'] = ['buffer']

  let g:deoplete#omni#functions = {}
  let g:deoplete#omni#functions.javascript = [
    \ 'tern#Complete'
  \]
  "let g:deoplete#ignore_sources = {'_': ['buffer', 'tags']} " reaalllly slow on big files
  "set completeopt=longest,menuone,preview
  set completeopt+=noinsert " autoselect
  "inoremap <expr> <TAB> deoplete#close_popup()
  inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
  inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
  "
  "call deoplete#custom#source('ultisnips', 'rank', 1000) " rank sources
  " Popup menu colors
  "highlight Pmenu ctermbg=8 guibg=#606060
  "highlight PmenuSel ctermbg=1 guifg=#dddd00 guibg=#1f82cd
  "highlight PmenuSbar ctermbg=0 guibg=#d6d6d6
  " DEBUG
  "let g:deoplete#enable_profile = 1
  "call deoplete#enable_logging('DEBUG', 'deoplete.log')
  "call deoplete#custom#source('ternjs', 'debug_enabled', 1)
  "Tern for vim
  "let g:tern#command = ['tern']
  "let g:tern#arguments = ['--persistent']

" File Types {
  "" Filetype detection
  augroup filetypedetect
    "" Detect .txt as 'text'
    autocmd! BufNewFile,BufRead *.txt setfiletype text
    " sass
    autocmd! BufNewFile,BufRead *.{sass,scss} setfiletype sass
    "" epub
    autocmd! BufReadCmd *.epub call zip#Browse(expand("<amatch>"))
    " markdown
    autocmd! BufNewFile,BufRead *.md setfiletype markdown
    autocmd! BufNewFile,BufRead *.mkd setfiletype markdown
    autocmd! BufNewFile,BufRead *.markdown setfiletype markdown
    " shell
    autocmd! BufNewFile,BufRead *.zsh-theme setfiletype zsh
    " javascript
    autocmd BufRead,BufNewFile *.es6 setfiletype javascript
    " ruby
    autocmd! BufNewFile,BufRead *.feature setfiletype cucumber
    " json
    autocmd! BufRead,BufNewFile .{eslintrc,babelrc} setf json
    " groovy
    autocmd! BufRead,BufNewFile *.gradle setf groovy
    " gitconfig
    autocmd! BufRead,BufNewFile gitconfig setf gitconfig
    " arduino
    au BufRead,BufNewFile *.pde set filetype=arduino
    au BufRead,BufNewFile *.ino set filetype=arduino
    " puppet
    au BufRead,BufNewFile *.pp set filetype=puppet
  augroup END
" }
