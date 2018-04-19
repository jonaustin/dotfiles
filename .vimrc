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

"" nvim-completion-manager
" Requires vim8 with has('python') or has('python3')
" Requires the installation of msgpack-python. (pip install msgpack-python)
if !has('nvim')
  Plug 'roxma/vim-hug-neovim-rpc' " compatibility layer for vim8
endif
Plug 'roxma/nvim-completion-manager'

"Plug 'roxma/nvim-cm-tern',  { 'do': 'npm install' }
"Plug 'ternjs/tern_for_vim', { 'do': 'npm install' }
"Plug 'mhartington/nvim-typescript', { 'do': 'npm install -g typescript' } " or tsuquyomi
"Plug 'roxma/ncm-rct-complete', { 'do': 'gem install rcodetools' }
"Plug 'Shougo/neco-syntax' " syntax completion

" Asynchronous execution library
Plug 'Shougo/vimproc.vim', {
      \ 'build' : {
      \     'windows' : 'tools\\update-dll-mingw',
      \     'cygwin' : 'make -f make_cygwin.mak',
      \     'mac' : 'make -f make_mac.mak',
      \     'unix' : 'make -f make_unix.mak',
      \    },
      \ }

" Ruby
Plug 'tpope/vim-haml'
Plug 'vim-ruby/vim-ruby'
Plug 'tpope/vim-rake'
Plug 'tpope/vim-rails'
"Plug 'ecomba/vim-ruby-refactoring'
"Plug 'slim-template/vim-slim'
"Plug 'depuracao/vim-rdoc'
"Plug 'hwartig/vim-seeing-is-believing' " e.g `#> 'foo'` or t9md/vim-ruby-xmpfilter (deprecated for Codi, but might still be useful)
"Plug 'tpope/vim-rvm' " switch ruby versions within vim

" Ruby testing
"Plug 'sunaku/vim-ruby-minitest'
"Plug 'jgdavey/tslime.vim' " needed for vim-rspec
"Plug 'thoughtbot/vim-rspec'
"Plug 'skwp/vim-rspec'
"Plug 'skalnik/vim-vroom' " ruby tests

" Javascript
Plug 'pangloss/vim-javascript'
"Plug 'jelera/vim-javascript-syntax'
"Plug 'othree/yajs.vim' " yet another javascript syntax
"Plug 'othree/javascript-libraries-syntax.vim'
"Plug 'vim-scripts/JavaScript-Indent'
"Plug 'kchmck/vim-coffee-script'
"Plug 'mmalecki/vim-node.js' " kind of rails.vim for node
"Plug 'mxw/vim-jsx'

" typescript
Plug 'leafgarland/typescript-vim' " syntax
Plug 'Quramy/vim-js-pretty-template' " template strings coloring
Plug 'jason0x43/vim-js-indent'
"Plug 'HerringtonDarkholme/yats.vim' " yet another typescript syntax
Plug 'Quramy/tsuquyomi' " tsserver (or nvim-typescript)

" Navigation
"Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
"Plug 'junegunn/fzf.vim'
Plug 'kien/ctrlp.vim'
Plug 'FelikZ/ctrlp-py-matcher'        " Exact filename matches!
Plug 'goldfeld/vim-seek'              " <leader>s<2 chars>
Plug 'mileszs/ack.vim'                " :Ack <search>
"Plug 'Lokaltog/vim-easymotion'        " <leader><leader>w
"Plug 'jeetsukumaran/vim-buffergator'  " <leader>b
"Plug 't9md/vim-choosewin'             " -

" Integrations
Plug 'skywind3000/asyncrun.vim' " used by other plugins to run things asynchronously (or :AsyncRun) Note: not compatible with vim-dispatch as it overrides :make
Plug 'janko-m/vim-test'
Plug 'metakirby5/codi.vim' " amazing repl
Plug 'rizzatti/dash.vim' " Dash.app integration - :<leader>d / :Dash (word under cursor), :Dash printf, :Dash setTimeout javascript, :DashKeywords backbone underscore javascript
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb' " Gbrowse for fugitive
Plug 'airblade/vim-gitgutter' " Show status in vim gutter
"Plug 'tpope/vim-git'
"Plug 'tpope/vim-eunuch' " :Mkdir, :SudoWrite, :Chmod, etc
"Plug 'mattn/gist-vim' "github gists
"Plug 'tmux-plugins/vim-tmux'
"Plug 'tbabej/taskwiki'
"Plug 'vimwiki/vimwiki', { 'branch': 'dev' } " required by taskwiki (taskwarrior)
Plug 'mboughaba/i3config.vim'

" Commands
Plug 'scrooloose/nerdcommenter'
Plug 'mbbill/undotree'
"Plug 'sjl/gundo.vim'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'junegunn/vim-easy-align' " :EasyAlign /<regex>/
Plug 'myusuf3/numbers.vim'
Plug 'w0rp/ale' " asynchronous linter
Plug 'xolox/vim-session' " e.g. :OpenSession :SaveSession
Plug 'xolox/vim-misc' " required by vim-session

" Colors
Plug 'w0ng/vim-hybrid'
"Plug 'sk1418/last256' " based on hybrid
"Plug 'Lokaltog/vim-distinguished'
""Plug 'jonaustin/vim-colors'
"Plug 'guns/jellyx.vim'
"Plug 'chriskempson/base16'
"Plug 'tomasr/molokai'
"Plug 'nanotech/jellybeans.vim'
"Plug 'vim-scripts/xoria256.vim'
"Plug 'ciaranm/inkpot'
"Plug 'jnurmine/Zenburn'
"Plug 'altercation/vim-colors-solarized'
"Plug 'sjl/badwolf' " clojure
"Plug 'morhetz/gruvbox'

" UI
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Misc
Plug 'wakatime/vim-wakatime'
Plug 'AD7six/vim-activity-log'


" Initialize plugin system
call plug#end()
" }}}

" Basics {
filetype plugin indent on " Automatically detect file types.
let mapleader = ","

if has('unix')
  if has('mac') " osx
    set clipboard=unnamed " * register -- SYSTEM (middle-click) clipboard (with --version +xterm_clipboard)
  else " linux, bsd, etc
    set clipboard=unnamedplus " >=7.3.74 only -- + register -- X11 (ctrl-c/v) clipboard
  endif
endif

set noautochdir " do not automatically change directory
"set cryptmethod=blowfish " strong blowfish encryption (instead of zip) - incomptabile with nvim
" }

" General {
syntax on " syntax highlighting
set mouse=a
set signcolumn=yes " Otherwise realtime linter gets annoying
" not every vim is compiled with this, use the following line instead
"autocmd BufEnter * if bufname("") !~ "^\[A-Za-z0-9\]*://" | lcd %:p:h | endif
scriptencoding utf-8
set encoding=utf-8 " no junk chars
set autowrite
set shortmess+=filmnrxoOtT " abbrev. of messages (avoids 'hit enter')
set foldmethod=syntax
set foldlevelstart=99
nnoremap <silent> <Space> @=(foldlevel('.')?'za':"\<Space>")<CR>
vnoremap <Space> zf

" Speed
set lazyredraw     " fix horrible slowdown issues when moving cursor with syntax on
set ttyfast        " assume fast connection (smoother redraw)
set synmaxcol=1024 " Syntax coloring lines that are too long just slows down the world
set nolist         " Hide invisibles
" Use the old regex engine
" http://stackoverflow.com/questions/16902317/vim-slow-with-ruby-syntax-highlighting
" set re=1 " TODO: remove this?

" Disable Ex mode
map Q <Nop>

" Setting up the directories {
set backup " backups are nice ...
set backupdir=$HOME/.vimbackup " but not when they clog .
set directory=$HOME/.vimswap " Same for swap files
set viewdir=$HOME/.vimviews " same but for view files
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
set incsearch      " find as you type search
set hlsearch       " highlight search terms
set winminheight=0 " windows can be 0 line high
set ignorecase     " case insensitive search
set smartcase      " become temporarily case sensitive when any uppercase letters present in search string
set undofile       " undo even after closing and re-opening a file!

" Formatting {
set wrap         " wrap long lines
set showbreak=↪  " prettier line wrap
set autoindent   " indent at the same level of the previous line
set shiftwidth=2 " use indents of 2 spaces
set expandtab    " tabs should be spaces for sanity

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

" remap to escape
inoremap jj <ESC>
inoremap jk <ESC>

" buffers
:noremap <leader>bn :bn<cr> " buffer next
:noremap <leader>bp :bp<cr> " buffer prev

" sudo write file
command Sudo :w !sudo tee %

" quit, write
map <C-s> :w<cr>
map <C-q> :q<cr>
map <leader>q   :q<cr>
map <leader>qq  :q!<cr>
map <leader>qqa :qall!<cr>
map <leader>w   :w<cr>
map <leader>wq  :wq<cr>

" quickfix
map <leader>qf :cope<cr> " open
map <leader>cf :ccl<cr> " close

" remove search highlights
map <silent><leader>. :nohl<cr>

" remove trailing whitespace
nnoremap <leader>W :%s/\s\+$//<cr>:let @/=''<CR>


""" PLUGINS """


" Airline/Powerline
let g:airline_powerline_fonts = 1
let g:airline_theme='base16'
let g:airline#extensions#tabline#enabled = 1

" vim-test
let g:test#javascript#jasmine#file_pattern = '\v.*/.*spec\.(js|jsx|coffee)$'
let g:test#ruby#rspec#executable = 'zeus rspec'
let test#strategy = 'neovim' "'neoterm'
nmap <silent> <leader>t :TestNearest<CR>
nmap <silent> <leader>T :TestFile<CR>
"nmap <silent> <leader>a :TestSuite<CR>
nmap <silent> <leader>l :TestLast<CR>
nmap <silent> <leader>g :TestVisit<CR>

" ALE asynchronous linter
" clear all fixers and linters
let g:ale_fixers = {}
let g:ale_linters = {}

if has('mac') " e.g. 'work'
  let g:ale_fixers['javascript'] = ['prettier']
  let g:ale_fix_on_save = 0
else
  let g:ale_linters['javascript'] = ['prettier_standard'] " npm i -g prettier-standard
  let g:ale_linters['javascript']  = ['']}
  "let g:ale_fixers['javascript'] = ['prettier']
  let g:ale_fix_on_save = 0 " probably enable this if using prettier-standard
endif

let g:ale_javascript_prettier_use_local_config = 1 " use local prettier config if available

" less distracting decorations
let g:ale_sign_error = '∙' " Less aggressive than the default '>>'
let g:ale_sign_warning = '◦'
let g:ale_lint_on_enter = 0 " Less distracting when opening a new file
let g:airline#extensions#ale#enabled = 1
let g:ale_set_highlights = 0 " Less distracting
highlight clear ALEErrorSign
highlight clear ALEWarningSign

" Codi repl
let g:codi#rightalign=0
let g:codi#width=80

" NERDCommenter
let g:NERDDefaultAlign = 'left' " put comments at col 0

" NERDTree
map <S-q> :NERDTreeToggle<cr>

" Ctrl-p
let g:ctrlp_custom_ignore = '\v[\/]node_modules$'

" Vim-session
let g:session_autoload = 'no'

""" END PLUGINS """


" Omni Completion
"set completeopt+=noinsert " autoselect
"set completeopt=menu,longest,preview
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

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
  " ruby autocomplete
  autocmd FileType ruby,eruby let g:rubycomplete_buffer_loading = 1
  autocmd FileType ruby,eruby let g:rubycomplete_classes_in_global = 1
  autocmd FileType ruby,eruby let g:rubycomplete_rails = 1
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

" Jump to previous line of file after closing and re-opening
" :help last-position-jump
autocmd BufReadPost *
      \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \ exe "normal g`\"" |
      \ endif

" Tips I always forget
" vertical split -> horizontal: ctrl+w then J
" horizontal split -> vertical: ctrl+w H or ctrl+w L
" reload all buffers - :bufdo e
" :w !sudo tee %
" gx - open link in browser
" :Ack <C-R><C-W> " use c-r/c-w to paste word under cursor into ex command prompt
" `. - go to last line edited / '' - go to start of last line edited
" g; / g, - jump through changelist (:help changelist)
" change all buffers to tabs - :tab sball
" gf in new tab: <c-w>gF - open in a new tab (Ctrl-w gF)
