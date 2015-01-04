" Modeline and Notes {
" vim: set sw=2 ts=2 sts=2 et tw=78 foldmarker={,} foldlevel=0 foldmethod=marker ft=vim
"
" }


" Vundle
	set nocompatible 		                " must be first directive
	filetype off                        " required for Vundle
  set rtp+=~/.vim/bundle/Vundle.vim
  call vundle#begin()

  " let Vundle manage Vundle
  Plugin 'gmarik/Vundle.vim'

  " Clojure
  Plugin 'kien/rainbow_parentheses.vim'
  Plugin 'guns/vim-clojure-highlight'
  Plugin 'guns/vim-clojure-static'
  Plugin 'tpope/vim-classpath'
  Plugin 'tpope/vim-fireplace' " ( nrepl middleware: https://github.com/clojure-emacs/cider-nrepl )
  Plugin 'tpope/vim-sexp-mappings-for-regular-people'
  Plugin 'guns/vim-sexp.git'

  " Language
  Plugin 'tpope/vim-liquid'
  Plugin 'cakebaker/scss-syntax.vim'
  Plugin 'ap/vim-css-color'
  Plugin 'mattn/emmet-vim'
  Plugin 'tpope/vim-markdown'
  " Ruby
  Plugin 'ecomba/vim-ruby-refactoring'
  Plugin 'tpope/vim-haml'
  Plugin 'vim-ruby/vim-ruby'
  Plugin 'slim-template/vim-slim'
  Plugin 'sunaku/vim-ruby-minitest'
  Plugin 'depuracao/vim-rdoc'
  Plugin 'hwartig/vim-seeing-is-believing' " or t9md/vim-ruby-xmpfilter
  Plugin 'tpope/vim-rvm' " so ruby uses rvm vim instead of system vim
  " Javascript
  Plugin 'pangloss/vim-javascript'
  Plugin 'jelera/vim-javascript-syntax'
  Plugin 'vim-scripts/JavaScript-Indent'
  Plugin 'othree/javascript-libraries-syntax.vim'
  Plugin 'kchmck/vim-coffee-script'
  Plugin 'mmalecki/vim-node.js' " detect node shebang and set FT to JS
  Plugin 'mtscout6/vim-cjsx'

  " Completion
  Plugin 'garbas/vim-snipmate' " Maybe replace with YCM-compatible Ultisnips
  Plugin 'honza/vim-snippets'
  Plugin 'Valloric/YouCompleteMe'
  Plugin 'marijnh/tern_for_vim' " JS
  "Plugin 'Shougo/neocomplete.vim'
  "Plugin 'ervandew/supertab' " Obsoleted by YCM
  "Plugin 'Raimondi/delimitMate' " http://oli.me.uk/2013/06/29/equipping-vim-for-javascript/


  " Navigation
  Plugin 'kien/ctrlp.vim'
  Plugin 'goldfeld/vim-seek'
  Plugin 'mileszs/ack.vim'
  Plugin 'Lokaltog/vim-easymotion'
  Plugin 'jeetsukumaran/vim-buffergator' " <leader>b
  Plugin 't9md/vim-choosewin'

  " Integrations
  Plugin 'scrooloose/nerdtree'
  Plugin 'matchit.zip'
  Plugin 'tpope/vim-fugitive'
  Plugin 'tpope/vim-git'
  Plugin 'tpope/vim-rails'
  Plugin 'tpope/vim-rake'
  Plugin 'thoughtbot/vim-rspec'
  "Plugin 'skwp/vim-rspec'
  Plugin 'tpope/vim-eunuch' " :Mkdir, :SudoWrite, :Chmod, etc
  "Plugin 'skalnik/vim-vroom' " ruby ruby tests

  " Commands
  Plugin 'scrooloose/nerdcommenter'
  Plugin 'mbbill/undotree'
  "Plugin 'sjl/gundo.vim'
  Plugin 'tpope/vim-repeat'
  Plugin 'tpope/vim-surround'
  Plugin 'vim-scripts/Tabmerge'
  "Plugin 'vim-scripts/sessionman.vim'
  Plugin 'xolox/vim-session'
  Plugin 'xolox/vim-misc' " required by vim-session
  Plugin 'jgdavey/tslime.vim'
  Plugin 'itspriddle/ZoomWin' " <c-w>-o
  Plugin 'mattn/webapi-vim'
  Plugin 'terryma/vim-multiple-cursors' " <c-n>
  Plugin 'thinca/vim-visualstar' " search your selection text in Visual-mode.
  "Plugin 'chrisbra/NrrwRgn'
  "Plugin 'rgarver/Kwbd.vim' " delete buffer without closing window

  " UI
  Plugin 'Lokaltog/powerline', {'rtp':'/powerline/bindings/vim'}
  Plugin 'airblade/vim-gitgutter'
  Plugin 'godlygeek/csapprox'
  Plugin 'junegunn/vim-easy-align'
  Plugin 'myusuf3/numbers.vim'
  Plugin 'scrooloose/syntastic'

  " Libs
  Plugin 'tomtom/tlib_vim'
  Plugin 'MarcWeber/vim-addon-mw-utils'

  " Colors
  Plugin 'w0ng/vim-hybrid'
  Plugin 'Lokaltog/vim-distinguished'
  "Plugin 'jonaustin/vim-colors'
  Plugin 'guns/jellyx.vim'
  "Plugin 'chriskempson/base16'
  Plugin 'tomasr/molokai'
  Plugin 'nanotech/jellybeans.vim'
  Plugin 'vim-scripts/xoria256.vim'
  Plugin 'ciaranm/inkpot'
  Plugin 'jnurmine/Zenburn'
  Plugin 'altercation/vim-colors-solarized'

  " Disabled
  "Plugin 'vim-scripts/scratch.vim'
  "Plugin 'amix/vim-zenroom'
  "Plugin 'chrisbra/csv.vim'
  "Plugin 'majutsushi/tagbar'
  "Plugin 'mattboehm/vim-unstack'
  "Plugin 'terryma/vim-multiple-cursors'
  "Plugin 'tpope/vim-abolish' " smarter subsitution - :%Subvert/facilit{y,ies}/building{,s}/g
  "Plugin 'tpope/vim-afterimage' " view Word/PDF files
  "Plugin 'tpope/vim-bundler'
  "Plugin 'tpope/vim-characterize' " inserting special chars (&copy) and getting unicode codes
  "Plugin 'tpope/vim-endwise'
  "Plugin 'tpope/vim-obsession' " probably conflicts with sessionman.vim
  "Plugin 'tpope/vim-unimpaired'

  " All of your Plugins must be added before the following line
  call vundle#end()                     " required

  " Turn back on after Vundle finishes its thing
  filetype plugin indent on  	        " Automatically detect file types.


" Basics {
	let mapleader = ","
  set clipboard+=unnamed               " * register -- SYSTEM (middle-click) clipboard (with --version +xterm_clipboard)
  ":set clipboard=unnamedplus         " >=7.3.74 only -- + register -- X11 (ctrl-c/v) clipboard
  set noautochdir                 " do not automatically change directory
  set cryptmethod=blowfish            " strong blowfish encryption (instead of zip)
" }

" General {
	syntax on 					                " syntax highlighting
	set mouse=a					                " disable mouse..add =a to enable
	" not every vim is compiled with this, use the following line instead
     "autocmd BufEnter * if bufname("") !~ "^\[A-Za-z0-9\]*://" | lcd %:p:h | endif
	scriptencoding utf-8
	set autowrite
	set shortmess+=filmnrxoOtT          " abbrev. of messages (avoids 'hit enter')
  set foldmethod=syntax
  set foldlevelstart=99
	" set spell 		 	     	            " spell checking on
  "set vb                             " visual bell, no beeping - disable - causes weird crap-glyphs in gnome-terminal

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
  " Disable K looking stuff up
  map K <Nop>


	" Setting up the directories {
  set backup 						            " backups are nice ...
  set backupdir=$HOME/.vimbackup    " but not when they clog .
  set directory=$HOME/.vimswap 	    " Same for swap files
  set viewdir=$HOME/.vimviews 	    " same but for view files

  " Creating directories if they don't exist
  silent execute '!mkdir -p $HOME/.vimbackup'
  silent execute '!mkdir -p $HOME/.vimswap'
  silent execute '!mkdir -p $HOME/.vimviews'
  " note these two below were causing rails.vim to not be able to find files
  " in 'path' so replaced with the autocmd
  "au BufWinLeave * silent! mkview  "make vim save view (state) (folds, cursor, etc)
  "au BufWinEnter * silent! loadview "make vim load view (state) (folds, cursor, etc)
  autocmd BufReadPost *
  \ if line("'\"") > 0 && line("'\"") <= line("$") |
  \ exe "normal g`\"" |
  \ endif
	" }
" }

" Vim UI {
	"color zenburn_jon
	"color leo
	set background=dark                 " Assume a dark background
  "let g:solarized_termcolors=256
  ""let g:solarized_termtrans=1
  "colo solarized
  "colo railscasts
  "colo hybrid
  colo base16-default

  "hi Normal ctermbg=232               " dark background

	set tabpagemax=15 				          " only show 15 tabs
	set showmode                   	    " display the current mode

	set cursorline  				            " highlight current line
	hi cursorline guibg=#333333 	      " highlight bg color of current line
	hi CursorColumn guibg=#333333       " highlight cursor

	if has('cmdline_info')
		set ruler                  	      " show the ruler
		set rulerformat=%30(%=\:b%n%y%m%r%w\ %l,%c%V\ %P%) " a ruler on steroids
		set showcmd                	      " show partial commands in status line and
									                    " selected characters/lines in visual mode
	endif

	if has('statusline')
		set laststatus=2           	      " show statusline always
		" Use the commented line if fugitive isn't installed
      "set statusline=%<%f\ %=\:\b%n%y%m%r%w\ %l,%c%V\ %P " a statusline, also on steroids
		set statusline=%<%f\ %h%m%r%y%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P
	endif

	set backspace=indent,eol,start 	   " Allow backspacing over indent, eol, and the start of an insert
	set linespace=0 				           " No extra spaces between rows
	set showmatch                  	   " show matching brackets/parenthesis
	set incsearch 					           " find as you type search
	set hlsearch 					             " highlight search terms
	set winminheight=0 				         " windows can be 0 line high
	set ignorecase 					           " case insensitive search
	set smartcase 					           " become temporarily case sensitive when any uppercase letters present in search string
	set wildmenu 					             " show list instead of just completing
	set wildmode=list:longest,full 	   " command <Tab> completion, list matches, then longest common part, then all.
	set whichwrap=b,s,h,l,<,>,[,]	     " backspace and cursor keys wrap to previous/next line
	set scrolljump=5 				           " lines to scroll when cursor leaves screen
	set scrolloff=10 				           " minimum lines to keep above and below cursor
	set foldenable  				           " auto fold code
	"set gdefault					             " the /g flag on :s substitutions by default
  "set relativenumber                " line numbers relative to current position
	set nu 							               " Line numbers on
  set undofile                       " undo even after closing and re-opening a file!
  set undodir=$HOME/.vimundo

  "set debug=msg                     " makes it so that error messages don't disappear after one second on startup.

" }

" Formatting {
	set wrap                      	   " wrap long lines
  set showbreak=↪                    " prettier line wrap
	set autoindent                 	   " indent at the same level of the previous line
	set shiftwidth=2               	   " use indents of 2 spaces
  set softtabstop=2                  " backspace will go back 2 chars instead of 1 (i.e. act like its a tab)
	set expandtab 	       	  	       " tabs should be spaces for sanity
	set tabstop=2 					           " an indentation every 2 columns
  set matchpairs+=<:>                " match, to be used with %
	set pastetoggle=<F10>          	   " pastetoggle (sane indentation on pastes)
  " Toggle paste mode
  nmap <silent> ,p :set invpaste<CR>:set paste?<CR>
	"set comments=sl:/*,mb:*,elx:*/    " auto format comment blocks
  set encoding=utf-8                 " no junk chars
  set textwidth=80
"  set colorcolumn=85                " show vertical colored column
  set shiftround                     " When at 3 spaces and I hit >>, go to 4, not 5.
  set formatoptions=qrn1             " q: Allow formatting of comments with gq
                                     " r: Automatically insert the current comment leader after hitting <Enter> in Insert mode.
                                     " n: When formatting text, recognize numbered lists.
                                     " 1: Don't break a line after a one-letter word.  It's broken before it instead (if possible).
                                     " l: Long lines are not broken in insert mode: When a line was longer than
                                     "    'textwidth' when the insert command started, Vim does not automatically format it.
" }

" Tricks {
  " Make the current window big, but leave others context {
    " https://www.destroyallsoftware.com/file-navigation-in-vim.html
    "set winwidth=84
    " We have to have a winheight bigger than we want to set winminheight. But if
    " we set winheight to be huge before winminheight, the winminheight set will
    " fail.
    "set winheight=5
    "set winminheight=5
    "set winheight=999
  " }
" }

" Key Mappings {

  " Aliases {
  :command! Wq wq
  :command! WQ wq
  :command! W w
  :command! Q q
  "}

  " Windows

  " For crosh
  "map <C-E> <C-W>
  "map <C-Q> <C-W>
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
  " move window to new tab
  map <leader>mt <C-W>T

  " split windows
  nnoremap <leader>sw <C-w>v<C-w>l " split and switch
  noremap <leader>o :only<cr>
  noremap <leader>O :only<cr>:tabonly<cr>

  " open ctag in new tab
  nmap <leader>ct <C-w><C-]><C-w>T

  " tabnew
  nmap <leader>tn :tabnew

  " add/remove numbers
  nmap <leader>qn :set nonu<cr>
  nmap <leader>an :set nu<cr>
  nmap <leader>nq :set nonu<cr>
  nmap <leader>na :set nu<cr>

  " copy current buffer to new tab
  " http://vim.wikia.com/wiki/Maximize_window_and_return_to_previous_split_structure
  nmap <leader>t% :tabedit %<CR>
  nmap <leader>td :tabclose<CR>

	" Yank from the cursor to the end of the line, to be consistent with C and D.
	nnoremap Y y$
  " yank all lines
  nmap <leader>ya :%y+<cr>

  " add new line without entering insert mode
  nmap <CR> o<Esc>

	" Change Working Directory to that of the current file
  "cmap cwd lcd %:p:h

	" remap jj to escape
	inoremap jj <ESC>
	inoremap jk <ESC>

  " Quickly toggle wrap mode (for the current window)
  nmap <leader>w :setlocal wrap!<CR>:setlocal wrap?<CR>

  " save / quit
  map <C-s> :w<cr>
  map <C-q> :q<cr>

  " sudo write file
  command Sudo :w !sudo tee %

  " buffers
    " buffer next
  :noremap <leader>bn :bn<cr>
    " buffer prev
  :noremap <leader>bp :bp<cr>

  " quit, write
  map <leader>q   :q<cr>
  map <leader>qq  :q!<cr>
  map <leader>qqa :qall!<cr>
  map <leader>w   :w<cr>
  map <leader>ww  :w!<cr>
  map <leader>wq  :wq<cr>

  " coding app calls
  map <leader>cuc   :!cucumber<cr>
  map <leader>cuco  :!cucumber %<cr>
  map <leader>rsp   :!rspec spec<cr>
  map <leader>rspo  :!rspec %<cr>
  map <leader>php   :!php %<cr>
  map <leader>phpl  :!php -l %<cr>

  " quickfix
  " open
  map <leader>qf :cope<cr>
  " close
  map <leader>cf :ccl<cr>

  " remove search highlights
  map <silent><leader>. :nohl<cr>

  """ Folding
  map <leader>f :fold<cr>
  " restore syntax folding
  map <silent><leader>zf :set foldmethod=syntax<CR>
  " This folds every line that does not contain the search pattern."}
  " see vimtip #282 and vimtip #108
  map <silent><leader>z :set foldexpr=getline(v:lnum)!~@/ foldlevel=0 foldcolumn=0 foldmethod=expr<CR>
  " this folds all classes and function to create a code index.
  " mnemonic: think "function fold"
  map zff :/^\s*class\s\\|^\s*function\s\\|^\s*def\s/<CR>:set foldmethod=expr foldlevel=0 foldcolumn=1<CR><CR>
  " space toggles the fold state under the cursor.
  nnoremap <silent><space> :exe 'normal! za'.(foldlevel('.')?'':'l')<cr>
  " <leader>space expands with zO
  map <leader><space> zO

  " mouse
  map <leader>m  :set mouse=a<cr>
  map <leader>mo :set mouse=<cr>

  " tab matches bracket pairs
  nnoremap <tab> %
  vnoremap <tab> %

  " get cmd mode with ;
  "nnoremap ; :

  " unfold with one hand
  nnoremap ;; zR

  " remove trailing whitespace
  nnoremap <leader>W :%s/\s\+$//<cr>:let @/=''<CR>

  " show/hide invisibles
  map <leader>I :set list<cr>
  map <leader>i :set nolist<cr>

  " The following beast is something i didn't write... it will return the
  " syntax highlighting group that the current 'thing' under the cursor
  " belongs to -- very useful for figuring out what to change as far as
  " syntax highlighting goes.
  nmap <silent> <leader>qq :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

  " Edit the vimrc file
  nmap <silent> <leader>ev :tabnew $MYVIMRC<CR>
  nmap <silent> <leader>sv :sp $MYVIMRC<CR>
  nnoremap <leader>vv <C-w><C-v><C-l>:e $MYVIMRC<cr> " vertical split
  nmap <leader>ssv :source $MYVIMRC<CR>


  " Make shift-insert work like in Xterm
  map <S-Insert> <MiddleMouse>
  map! <S-Insert> <MiddleMouse>
" }

" Autocomplete
" Ruby
autocmd FileType ruby,eruby let g:rubycomplete_buffer_loading = 1
autocmd FileType ruby,eruby let g:rubycomplete_classes_in_global = 1
autocmd FileType ruby,eruby let g:rubycomplete_rails = 1

" Plugins {
  " vim-session
  let g:session_autoload = 'no'

  " syntastic
  let g:syntastic_html_tidy_ignore_errors=[" proprietary attribute " ,"trimming empty <", "unescaped &" , "lacks \"action", "is not recognized!", "discarding unexpected"]

  " choosewin
  nmap  -  <Plug>(choosewin)

  " vim-seeing-is-believing
  nmap <buffer> <F5> <Plug>(seeing-is-believing-run)
  xmap <buffer> <F5> <Plug>(seeing-is-believing-run)
  imap <buffer> <F5> <Plug>(seeing-is-believing-run)

  nmap <buffer> <F4> <Plug>(seeing-is-believing-mark)
  xmap <buffer> <F4> <Plug>(seeing-is-believing-mark)
  imap <buffer> <F4> <Plug>(seeing-is-believing-mark)

	" NERDTree
	map <S-q> :NERDTreeToggle<cr>

	" TagList
	map <leader>tl :TlistToggle<cr>

  " Supertab {
  "let g:SuperTabDefaultCompletionType = "context"
  "let g:SuperTabContextDefaultCompletionType = "<c-x><c-o>"
  " }

  " Misc {
  ":map <C-F10> <Esc>:vsp<CR>:VTree<CR>
  " map Control + F10 to Vtree

  let g:checksyntax_auto = 1

  "comment out line(s) in visual mode
  vmap  o  :call NERDComment(1, 'toggle')<CR>
  let g:NERDShutUp=1

  let b:match_ignorecase = 1
  " }

  " OmniComplete {
  "if has("autocmd") && exists("+omnifunc")
  "autocmd Filetype *
  "\if &omnifunc == "" |
  "\setlocal omnifunc=syntaxcomplete#Complete |
  "\endif
  "endif

  " Popup menu hightLight Group
  "highlight Pmenu 	ctermbg=13 	guibg=DarkBlue
  highlight PmenuSel 	ctermbg=7 	guibg=DarkBlue 		guifg=LightBlue
  "highlight PmenuSbar ctermbg=7 	guibg=DarkGray
  "highlight PmenuThumb 			guibg=Black

  hi Pmenu  guifg=#000000 guibg=#F8F8F8 ctermfg=black ctermbg=Lightgray
  hi PmenuSbar  guifg=#8A95A7 guibg=#F8F8F8 gui=NONE ctermfg=darkcyan ctermbg=lightgray cterm=NONE
  hi PmenuThumb  guifg=#F8F8F8 guibg=#8A95A7 gui=NONE ctermfg=lightgray ctermbg=darkcyan cterm=NONE

  " some convenient mappings
  " commented as this was causing pumvisible()... to show up every time enter is hit..
  "inoremap <expr> <Esc>      pumvisible() ? "\<C-e>" : "\<Esc>"
  "inoremap <expr> <CR>       pumvisible() ? "\<C-y>" : "\<CR>"
  "inoremap <expr> <Down>     pumvisible() ? "\<C-n>" : "\<Down>"
  "inoremap <expr> <Up>       pumvisible() ? "\<C-p>" : "\<Up>"
  "inoremap <expr> <C-d> 	   pumvisible() ? "\<PageDown>\<C-p>\<C-n>" : "\<C-d>"
  "inoremap <expr> <C-u>      pumvisible() ? "\<PageUp>\<C-p>\<C-n>" : "\<C-u>"

  " automatically open and close the popup menu / preview window
  au CursorMovedI,InsertLeave * if pumvisible() == 0|silent! pclose|endif
  set completeopt=menu,longest,preview
  " }

  " SnipMate {
  " Setting the author var
  let g:snips_author = 'Jon Austin <jon.i.austin@gmail.com>'
  " Remap from tab to avoid conflicts with YCM
  imap <C-J> <Plug>snipMateNextOrTrigger
  smap <C-J> <Plug>snipMateNextOrTrigger
  " }

  " dbext {
  "let g:dbext_default_SQLITE_bin='sqlite3'
  " }

  " Start interactive EasyAlign in visual mode
  vmap <Enter> <Plug>(EasyAlign)
  " Start interactive EasyAlign with a Vim movement
  nmap <Leader>a <Plug>(EasyAlign)

  " Vim-rspec
  let g:rspec_command = 'call Send_to_Tmux("rspec {spec}\n")'
  map <Leader>t :call RunCurrentSpecFile()<CR>
  map <Leader>T :call RunNearestSpec()<CR>
  "map <Leader>l :call RunLastSpec()<CR>
  "map <Leader>a :call RunAllSpecs()<CR>

  " use silver_searcher instead of ack
  let g:ackprg = 'ag --nogroup --nocolor --column'

  " YouCompleteMe
  let g:ycm_add_preview_to_completeopt = 1
  let g:ycm_collect_identifiers_from_tags_files = 1 " ctags
  let g:ycm_filetype_blacklist = { 'ruby' : 1 } " will not work with ruby files (segfault) so just disable. egh.
  " blargh - Tab doesn't work for some reason
  "let g:ycm_auto_trigger = 0
  "let g:ycm_key_invoke_completion = '<TAB>'

  " Clojure
  " RainbowParentheses
  au BufEnter *.clj RainbowParenthesesActivate
  au Syntax clojure RainbowParenthesesLoadRound
  au Syntax clojure RainbowParenthesesLoadSquare
  au Syntax clojure RainbowParenthesesLoadBraces
" }

  " GUI Settings {
  " GVIM- (here instead of .gvimrc)
  if has('gui_running')
    set guioptions-=T          	" remove the toolbar
    set lines=40               	" 40 lines of text instead of 24,
  endif
  " }

  " Windows Compatible {
  " On Windows, also use '.vim' instead of 'vimfiles'; this makes synchronization
  " across (heterogeneous) systems easier.
  if has('win32') || has('win64')
    set runtimepath=$HOME/.vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,$HOME/.vim/after
  endif
  " }

  " Ack {
  "map <leader>a :Ack<space>
  "}

  " Gundo {
  "nnoremap <S-U> :GundoToggle<cr>
  " }

  " Fugitive {
  nnoremap <leader>gb :Gblame<cr>
  nnoremap <leader>gd :Gdiff<cr>
  " }

  " Turbux {
  let g:turbux_command_prefix = 'bundle exec'
  " }

  " Vimux {
  " https://github.com/benmills/vimux/
  " Config
  " Use exising pane (not used by vim) if found instead of running split-window.
  let VimuxUseNearestPane = 1
  let g:VimuxOrientation = "v"

  " open rails console
  map <Leader>vc :call VimuxRunCommand("clear; bundle exec rails c")<CR>:VimuxSwitchToRunner<CR>

  " Prompt for a command to run
  map <Leader>vp :VimuxPromptCommand<CR>

  " Prompt for a command to run
  map <Leader>vl :VimuxRunLastCommand<CR>

  "Move into the tmux runner pane created by `VimuxRunCommand` and enter copy
  "(scroll) mode
  " Inspect runner pane
  map <Leader>vi :VimuxInspectRunner<CR>

  " switch to runner pane
  map <Leader>vr :VimuxSwitchToRunner<CR>

  " Close vim tmux runner opened by VimuxRunCommand
  map <Leader>vq :VimuxCloseRunner<CR>

  " Close all other tmux panes in current window
  map <Leader>vx :VimuxClosePanes<CR>

  " If text is selected, save it in the v buffer and send that buffer it to tmux
  vmap <LocalLeader>vs "vy :call VimuxRunCommand(@v . "\n", 0)<CR>

  " Select current paragraph and send it to tmux
  nmap <LocalLeader>vs vip<LocalLeader>vs<CR>

  " Ctrl-P {{{
  :let g:ctrlp_match_window_bottom = 0
  :let g:ctrlp_match_window_reversed = 0
  :let g:ctrlp_working_path_mode = 0
  :let g:ctrlp_dotfiles = 0
  :let g:ctrlp_match_window = 'max:10,results:20'
  "}}}

" Various {
  if has("autocmd")
    " Restore cursor position (initially for IRB<->Vim integration, if obnoxious for other things, put in ftdetect or somesuch)
    autocmd BufReadPost *
          \ if line("'\"") > 1 && line("'\"") <= line("$") |
          \   exe "normal! g`\"" |
          \ endif
  endif
  set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*/coverage/*
" }

" Testing {
  "set scrolloff=999 " causes current line to always be vertically centered
  "(unfortunately really screws up selecting with mouse)
" }

" File Types {
  "" Filetype detection
  augroup filetypedetect
    "" Detect .txt as 'text'
    autocmd! BufNewFile,BufRead *.txt setfiletype text
    "" cakephp
    autocmd! BufNewFile,BufRead *.thtml setfiletype php
    autocmd! BufNewFile,BufRead *.ctp setfiletype php
    "" sass
    autocmd! BufNewFile,BufRead *.scss setfiletype sass
    "" epub
    autocmd! BufReadCmd   *.epub      call zip#Browse(expand("<amatch>"))
    "" markdown
    autocmd! BufNewFile,BufRead *.md setfiletype markdown
    autocmd! BufNewFile,BufRead *.mkd setfiletype markdown
    autocmd! BufNewFile,BufRead *.markdown setfiletype markdown
    autocmd! BufNewFile,BufRead *.feature setfiletype cucumber
    "" shell
    autocmd! BufNewFile,BufRead *.zsh-theme setfiletype zsh
  augroup END
" }

" Languages {
  " Ruby
  nmap <leader>rci :%!ruby-code-indenter<cr>
  map <leader>sqf :Rcd<cr>:!sort -u tmp/quickfix > tmp/quickfix.sort<cr>:cfile tmp/quickfix.sort<cr>
  map <leader>sc  :!ruby -c %<cr>

  command! FR set filetype=ruby
  " Nab lines from ~/.pry_history (respects "count": `,20ph`)
  nmap <Leader>ph :<c-u>let pc = (v:count1 ? v:count1 : 1)<cr>:read !tail -<c-r>=pc<cr> ~/.pry_history<cr>:.-<c-r>=pc-1<cr>:norm <c-r>=pc<cr>==<cr>

  " Rails.vim extensions
  " Edit routes
  command! Rroutes :R config/routes.rb
  command! RTroutes :RT config/routes.rb

  " Edit factories
  command! Rfactories :R spec/support/factories.rb
  command! RTfactories :RT spec/support/factories.rb

  " Edit mocks
  command! Rmocks :R spec/support/mocks.rb
  command! RTmocks :RT spec/support/mocks.rb

  map <leader>gr :topleft :split config/routes.rb<cr>
  function! ShowRoutes()
    " Requires 'scratch' plugin
    :topleft 100 :split __Routes__
    " Make sure Vim doesn't write __Routes__ as a file
    :set buftype=nofile
    " Delete everything
    :normal 1GdG
    " Put routes output in buffer
    :0r! bundle exec rake -s routes
    " Size window to number of lines (1 plus rake output length)
    :exec ":normal " . line("$") . "_ "
    " Move cursor to bottom
    :normal 1GG
    " Delete empty trailing line
    :normal dd
  endfunction
  map <leader>gR :call ShowRoutes()<cr>

  function! PromoteToLet()
    :normal! dd
    " :exec '?^\s*it\>'
    :normal! P
    :.s/\(\w\+\) = \(.*\)$/let(:\1) { \2 }/
    :normal ==
  endfunction
  :command! PromoteToLet :call PromoteToLet()
  :map <leader>p :PromoteToLet<cr>

  function! UpdateHashSyntax()
    :normal! H
    :%s/:\([^ ]*\)\(\s*\)=>/\1:/g
    :normal ==
  endfunction
  :command! UpdateHashSyntax :call UpdateHashSyntax()
  :map <leader>H :UpdateHashSyntax<cr>

"}


" Other Customizations {
  " Add #s to tabline so gt/gT is actually useful..sheesh.. {
  set showtabline=1         " 0, 1 or 2; when to use a tab pages line

  " relative/absolute line number switching
  function! NumberToggle()
    if(&relativenumber == 1)
      set number
    else
      set relativenumber
    endif
  endfunc

  nnoremap <C-n> :call NumberToggle()<cr>

  " automatically switch to absolute line numbers whenever vim loses focus
  " (this doesn't seem to work...)
  ":au FocusLost * :set number
  ":au FocusGained * :set relativenumber

  " insert mode: automatically use absolute line numbers
  "autocmd InsertEnter * :set number
  " command mode: automatically use relative line numbers
  "autocmd InsertLeave * :set relativenumber
  " }

  "" Strip trailing whitespace {
  "function! <SID>StripTrailingWhitespaces()
    "" Preparation: save last search, and cursor position.
    "let _s=@/
    "let l = line(".")
    "let c = col(".")
    "" Do the business:
    "%s/\s\+$//e
    "" Clean up: restore previous search history, and cursor position
    "let @/=_s
    "call cursor(l, c)
  "endfunction

  "if has("autocmd")
    "autocmd Filetype html :call <SID>StripTrailingWhitespaces()
    "autocmd Filetype ruby :call <SID>StripTrailingWhitespaces()
  "endif
  " }

  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " OpenChangedFiles COMMAND
  " Open a split for each dirty file in git
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  function! OpenChangedFiles()
    only " Close all windows, unless they're modified
    let status = system('git status -s | grep "^ \?\(M\|A\|UU\)" | sed "s/^.\{3\}//"')
    let filenames = split(status, "\n")
    exec "edit " . filenames[0]
    for filename in filenames[1:]
      exec "sp " . filename
    endfor
  endfunction
  command! OpenChangedFiles :call OpenChangedFiles()

  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " RemoveFancyCharacters COMMAND
  " Remove smart quotes, etc.
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  function! RemoveFancyCharacters()
      let typo = {}
      let typo["“"] = '"'
      let typo["”"] = '"'
      let typo["‘"] = "'"
      let typo["’"] = "'"
      let typo["–"] = '--'
      let typo["—"] = '---'
      let typo["…"] = '...'
      :exe ":%s/".join(keys(typo), '\|').'/\=typo[submatch(0)]/ge'
  endfunction
  command! RemoveFancyCharacters :call RemoveFancyCharacters()

  " Rails ctags {
  let g:rails_ctags_arguments='--exclude="*.js" --regex-Ruby=/\(scope\|has_many\|has_and_belongs_to_many\|belongs_to\)\ :\([A-z]\+\)\ *,/\\2/e --exclude="*.sql" --exclude=.git --exclude=log --exclude=tmp --exclude=import --exclude=spec'
  " }

  " Ruby folding {
    function! RubyFold()
      if (exists("b:ruby_folded"))
        return
      endif
      let b:ruby_folded = 1

      setlocal foldenable
      setlocal expandtab
      setlocal foldmethod=syntax
      execute 'silent! %foldopen!'
      execute  'silent g/\v^\s+(def |it .+do|task |Factory |context .+do|describe .+do)/foldc'
    endfunction

    if has("autocmd")
      "autocmd Filetype rspec :call RubyFold()
      "autocmd Filetype ruby :call RubyFold()
    endif
    command! RubyFold :call RubyFold()
  " }
" }

" Functions {{{
  " Merge a tab into a split in the previous window
  function! MergeTabs()
    if tabpagenr() == 1
      return
    endif
    let bufferName = bufname("%")
    if tabpagenr("$") == tabpagenr()
      close!
    else
      close!
      tabprev
    endif
    split
    execute "buffer " . bufferName
  endfunction
  nmap <C-W>u :call MergeTabs()<CR>
"}}}
