""" BASE """
" Use Vim settings, rather then Vi settings
" This must be first, because it changes other options as a side effect
set nocompatible
set showmode  "show when i'm in insert/overlay mode
set showcmd   "when a command is in progress, show it in the status bar (or size of selection in Visual mode)
set showmatch "show matches for braces, parens, etc
set spelllang=en_us

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if (&t_Co > 2 || has("gui_running")) && !exists("syntax_on")
    syntax on
    " highlight search results
    set hlsearch
endif

" Enable loading filetype and indentation plugins
filetype on
filetype plugin on
filetype indent on

set tabstop=2
set shiftwidth=2

set expandtab
set smarttab

" just use `filetype indent on` -- should work better than auto and/or smart
" -- may depend on language though...
"set autoindent
"set smartindent
"set cindent

" case only matters with mixed case expressions
set ignorecase
set smartcase

set nu " show line numbers

"for 256 colors
set t_Co=256

" Use UTF-8 as the default buffer encoding
set enc=utf-8

" Use F5 to toggle 'paste' mode
set pastetoggle=<F10>

" Enable folding by fold markers
set foldmethod=marker 

" Autoclose folds, when moving out of them
set foldclose=all

" Use incremental searching
set incsearch


" Jump 5 lines when running out of the screen -- for slow connections --  :help scrolljump
set scrolljump=5

" Scroll when cursor gets within 3 characters of top/bottom edge
set scrolloff=10
"
" Write swap file to disk after every 50 characters
set updatecount=50

colo zenburn_jon

set mouse=a

""" note: completeopt:longest may not work well with `set showcmd` (see help showcmd 


" Show info in ruler
set ruler
" enable set statusline (i.e. statusbar)
set laststatus=2

" Remember things between sessions
"
" '20  - remember marks for 20 previous files
" \"50 - save 50 lines for each register
" :200  - remember 20 items in command-line history 
" %    - remember the buffer list (if vim started without a file arg)
" n    - set name of viminfo file
set viminfo='20,\"50,:200,%,n~/.viminfo

" Allow file inline modelines to provide settings
" i.e. vi:noai:sw=3 ts=6
set modeline

"set bg=dark
"
" allow backspacing over everything in insert mode (i.e. backspace at start of
" line will go to end of previous line)
set backspace=start,eol,indent

" Use menu to show command-line completion (in 'full' case) -- tab completion
" at the : command line
set wildmenu
" Set command-line completion mode:
"   - on first <Tab>, when more than one match, list all matches and complete
"     the longest common  string
"   - on second <Tab>, complete the next full match and show menu
set wildmode=list:longest,full

set ruler< "turn on rule (status info) at the bottom of screen

runtime ftplugin/man.vim "turn on man pages :Man



""" Compilers
compiler ruby         " Enable compiler support for ruby



""" BASE (that i don't really understand)

" for omni completion i believe
set complete-=k complete+=k
set completeopt=menu,longest,preview


" Go back to the position the cursor was on the last time this file was edited
au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$")|execute("normal `\"")|endif

" netRW: Open files in a split window
let g:netrw_browse_split = 1

"set statusline=%F%m%r%h%w\ [format=%{&ff}]\ [type=%y]\ [pos=%04l,%04v][%p%%]\ [len=%L] " [ASCII=\%03.3b]\ [HEX=\%02.2B]\ 
set statusline=%F%m%r%h%w\ format=%{&ff}\ type=%y\ pos=%04l,%04v\ %p%%\ len=%L " [ASCII=\%03.3b]\ [HEX=\%02.2B]\ 









""" MAPPINGS

" save changes
map ,w :w<CR>
map ,s :w<CR>
" exit vim without saving any changes
map ,q :q!<CR>
" exit vim saving changes
map ,x :x<CR>

" No Help, please
nmap <F1> <Esc>

" This is totally awesome - remap jj to escape
" in insert mode. You’ll never type jj anyway,
" so it’s great!
inoremap jj <ESC>


" use shift <F6> to toggle line numbers
"nmap <silent> <F6> :set number!<CR>

" Map go in and back for definition to arrow keys
map <silent><C-Right> <C-]>
map <silent><C-Left> <C-T>

" open filename under cursor in a new window (use current file's working
" directory) 
nmap gf :new %:p:h/<cfile><CR>

" page down with <Space>
nmap <Space> <PageDown>
nmap <C-Down> <PageDown>
nmap <C-Up> <PageUp>
nmap <C-Space> <PageUp>



""" MAPPINGS (that i don't really understand)

" map ,f to display all lines with keyword under cursor and ask which one to
" jump to
nmap ,f [I:let nr = input("Which one: ")<Bar>exe "normal " . nr ."[\t"<CR>


" switch to upper/lower window quickly
"map <C-J> <C-W>j
"map <C-K> <C-W>k
" use CTRL-F for omni completion
"imap <C-F> 






""" PLUGINS

" NERDTree
map <S-w> :NERDTreeToggle<cr>

" TagList
map <S-t> :TlistToggle<cr>

" GetLatestVimScripts
let g:GetLatestVimScripts_allowautoinstall=1

" auto switch to folder where editing file
autocmd BufEnter * cd %:p:h

" CloseTag
"autocmd FileType html,xhtml,xml source ~/.vim/scripts/closetag.vim  

" Snippets are activated by Shift+Tab
let g:snippetsEmu_key = "<S-Tab>"



""" File types
augroup mkd
  autocmd BufRead *.mkd  set ai formatoptions=tcroqn2 comments=n:>
augroup END

au FileType text setlocal tw=150







""" HACKS



" fixdel for screen
"if &term == "screen-256color"
"    set t_kb=
"    fixdel
"endif

" Fix my <Backspace> key (in Mac OS X Terminal & screen [& debian for some
" reason]) -- REMEMBER -- use Ctrl-V-? -- i.e. hold down Ctrl,v,shift,/ all at
" the same time!
"set t_kb=
"fixdel


""" CODING """

if !exists('*Wordpress_vim')
    runtime vimblog.vim
endif



""" Source
so ~/.vim/abbreviations.vim

""" csound
au BufNewFile,BufRead *.orc,*.sco,*.csd   so ~/.vim/syntax/csound.vim
au BufNewFile,BufRead *.csd               so ~/.vim/macros/csound_macros.vim
au BufNewFile *.csd                       0r ~/.vim/templates/template.csd
au BufNewFile *.orc                       0r ~/.vim/templates/template.orc
