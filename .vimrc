" Modeline and Notes {
" vim: set foldmarker={,} foldlevel=0 foldmethod=marker spell syntax=vim:
"
" }

" Pathogen {
  call pathogen#infect() 
" }

" Basics {
	set nocompatible 		                " must be first line
	set background=dark                 " Assume a dark background
	let mapleader = ","
  set clipboard=unnamed               " * register -- SYSTEM (middle-click) clipboard (with --version +xterm_clipboard)
  ":set clipboard=unnamedplus         " >=7.3.74 only -- + register -- X11 (ctrl-c/v) clipboard
" }

" General {
  runtime! macros/matchit.vim
	filetype plugin indent on  	        " Automatically detect file types.
	syntax on 					                " syntax highlighting
	set mouse=a					                " disable mouse..add =a to enable
	" not every vim is compiled with this, use the following line instead
     "autocmd BufEnter * if bufname("") !~ "^\[A-Za-z0-9\]*://" | lcd %:p:h | endif
	scriptencoding utf-8
	set autowrite
	set shortmess+=filmnrxoOtT          " abbrev. of messages (avoids 'hit enter')
  set foldmethod=syntax
  set foldlevelstart=2
	" set spell 		 	     	            " spell checking on
  "set vb                              " visual bell, no beeping - disable - causes weird crap-glyphs in gnome-terminal

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
  colo railscasts

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
	set smartcase 					           " become temporarilly case sensitive when any uppercase letters present in search string
	set wildmenu 					             " show list instead of just completing
	set wildmode=list:longest,full 	   " comand <Tab> completion, list matches, then longest common part, then all.
	set whichwrap=b,s,h,l,<,>,[,]	     " backspace and cursor keys wrap to previous/next line
	set scrolljump=5 				           " lines to scroll when cursor leaves screen
	set scrolloff=10 				           " minimum lines to keep above and below cursor
	set foldenable  				           " auto fold code
	"set gdefault					             " the /g flag on :s substitutions by default
  "set relativenumber                " line numbers relative to current position
	set nu 							               " Line numbers on
  set undofile                       " undo even after closing and re-opening a file!
  set undodir=$HOME/.vimundo 
  set ttyfast                        " assume fast connection (smoother redraw)
  "set debug=msg                     " makes it so that error messages don't disappear after one second on startup.

" }

" Formatting {
	set wrap                      	   " wrap long lines
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
  set formatoptions=qrn1             " q: Allow formatting of comments with gq
                                     " r: Automatically insert the current comment leader after hitting <Enter> in Insert mode.
                                     " n: When formatting text, recognize numbered lists.
                                     " 1: Don't break a line after a one-letter word.  It's broken before it instead (if possible). 
                                     " l: Long lines are not broken in insert mode: When a line was longer than 
                                     "    'textwidth' when the insert command started, Vim does not automatically format it.
" }

" Key Mappings {

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

  " add/remove numbers
  map <leader>qn :set nonu<cr>
  map <leader>an :set nu<cr>

	" Yank from the cursor to the end of the line, to be consistent with C and D.
	nnoremap Y y$

	" Shortcuts
	" Change Working Directory to that of the current file
  "cmap cwd lcd %:p:h

	" remap jj to escape
	inoremap jj <ESC>
	inoremap jk <ESC>

  " split windows
  nnoremap <leader>sw <C-w>v<C-w>l " split and switch
  noremap <leader>o :only<cr>
  noremap <leader>O :only<cr>:tabonly<cr>

  " open ctag in new tab
  nmap <leader>ct <C-w><C-]><C-w>T

  " buffers
    " buffer next
  :noremap <leader>bn :bn<cr>
    " buffer prev
  :noremap <leader>bp :bp<cr>

	" NERDTree
	map <S-q> :NERDTreeToggle<cr>

	" TagList
	map <leader>tl :TlistToggle<cr>

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

  " Blog
  map <leader>bl :BlogList<cr>

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
  nnoremap ; :

  " remove trailing whitespace
  nnoremap <leader>W :%s/\s\+$//<cr>:let @/=''<CR>

  " open vimrc in split window
  nnoremap <leader>ev <C-w><C-v><C-l>:e $MYVIMRC<cr>

  " show/hide invisibles
  map <leader>I :set list<cr>
  map <leader>i :set nolist<cr>

  " yank all lines
  map <leader>ya :%y+<cr>

  " The following beast is something i didn't write... it will return the
  " syntax highlighting group that the current 'thing' under the cursor
  " belongs to -- very useful for figuring out what to change as far as
  " syntax highlighting goes.
  nmap <silent> <leader>qq :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

  " Edit the vimrc file
  nmap <silent> ,ev :e $MYVIMRC<CR>
  nmap <silent> ,sv :so $MYVIMRC<CR>


  " Make shift-insert work like in Xterm
  map <S-Insert> <MiddleMouse>
  map! <S-Insert> <MiddleMouse>
" }

" Plugins {

  " Supertab {
  "let g:SuperTabDefaultCompletionType = "context"
  let g:SuperTabContextDefaultCompletionType = "<c-x><c-o>"
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
  " Shortcut for reloading snippets, useful when developing
  nnoremap ,smr <esc>:exec ReloadAllSnippets()<cr>
  " }

  " dbext {
  let g:dbext_default_SQLITE_bin='sqlite3'
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
  map <leader>a :Ack<space>
  "}

  " Gundo {
  nnoremap <S-U> :GundoToggle<cr>
  " }
" }

" Various {
  if has("autocmd")
    " Restore cursor position (initially for IRB<->Vim integration, if obnoxious for other things, put in ftdetect or somesuch)
    autocmd BufReadPost *
          \ if line("'\"") > 1 && line("'\"") <= line("$") |
          \   exe "normal! g`\"" |
          \ endif
  endif
" }

" Testing {
  "set scrolloff=999 " causes current line to always be vertically centered
  "(unforuntately really screws up selecting with mouse)
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

  " Strip trailing whitespace {
  function! <SID>StripTrailingWhitespaces()
    " Preparation: save last search, and cursor position.
    let _s=@/
    let l = line(".")
    let c = col(".")
    " Do the business:
    %s/\s\+$//e
    " Clean up: restore previous search history, and cursor position
    let @/=_s
    call cursor(l, c)
  endfunction

  if has("autocmd")
    autocmd Filetype html :call <SID>StripTrailingWhitespaces()
    autocmd Filetype ruby :call <SID>StripTrailingWhitespaces()
  endif
  " }

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
  " }
" }

" LS {
map <leader>rs :Rcd<cr>:!sort -u tmp/quickfix > tmp/quickfix.sort<cr>:cfile tmp/quickfix.sort<cr>
" }

