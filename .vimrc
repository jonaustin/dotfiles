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

if has('nvim')
  Plug 'Shougo/denite.nvim', { 'do': ':UpdateRemotePlugins' }
  set pyx=3
else
  Plug 'Shougo/denite.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
endif

"Plug 'lambdalisue/vim-pyenv'

" Completions
" https://stackoverflow.com/a/22253548/617320
"Plug 'Valloric/YouCompleteMe'
Plug 'neoclide/coc.nvim', {'do': 'yarn install --frozen-lockfile'}
Plug 'liuchengxu/vista.vim' " LSP viewer/finder :Vista
Plug 'reedes/vim-lexical' " spelling/dictionary completion
Plug 'metalelf0/supertab' " Plug 'ervandew/supertab'
"Plug 'SirVer/ultisnips' " C-w, c-b, c-x -- <leader><tab>; c-y to trigger?
Plug 'honza/vim-snippets'
"Plug 'ternjs/tern_for_vim', { 'do': 'npm install' } " also `npm i -g tern` ### Not needed with youcompleteme: https://github.com/Valloric/YouCompleteMe/pull/1849
"Plug 'mhartington/nvim-typescript', { 'do': 'npm install -g typescript' } " or tsuquyomi
"Plug 'roxma/ncm-rct-complete', { 'do': 'gem install rcodetools' }
"Plug 'Shougo/neco-syntax' " syntax completion
Plug 'mattn/emmet-vim' " div<c-y>,

" Asynchronous execution library
Plug 'Shougo/vimproc.vim', {
      \ 'build' : {
      \     'windows' : 'tools\\update-dll-mingw',
      \     'cygwin' : 'make -f make_cygwin.mak',
      \     'mac' : 'make -f make_mac.mak',
      \     'unix' : 'make -f make_unix.mak',
      \    },
      \ }

" General syntax
Plug 'rodjek/vim-puppet'
Plug 'pearofducks/ansible-vim'
Plug 'Glench/Vim-Jinja2-Syntax'
Plug 'hashivim/vim-terraform'

" Ruby
Plug 'tpope/vim-haml'
Plug 'vim-ruby/vim-ruby'
Plug 'tpope/vim-rake'
Plug 'tpope/vim-rails'
"Plug 'ecomba/vim-ruby-refactoring'

" Golang
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'sebdah/vim-delve'
" https://hackernoon.com/my-neovim-setup-for-go-7f7b6e805876
"  ]] takes you to the next function or method
"  [[ takes you to the previous function or method
"  shift-K to get docs for a func
au FileType go set noexpandtab
au FileType go set shiftwidth=4
au FileType go set softtabstop=4
au FileType go set tabstop=4
let g:go_auto_type_info = 1 " show actual data type in status line when hovered
let g:go_highlight_build_constraints = 1
let g:go_highlight_extra_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_operators = 1
let g:go_highlight_structs = 1
let g:go_highlight_types = 1
let g:go_auto_sameids = 1 " cursor over one variable will highlight other uses of that var
let g:go_fmt_command = "goimports" " automatically import dependencies after safe
" Error and warning signs.
"let g:ale_sign_error = '⤫'
"let g:ale_sign_warning = '⚠'
au FileType go nmap <leader>gt :GoDeclsDir<cr> " show all funcs in file
au FileType go nmap <F12> <Plug>(go-def) " go to definition of func
" switch between file and tests
" au Filetype go nmap <leader>ga <Plug>(go-alternate-edit)
" au Filetype go nmap <leader>gah <Plug>(go-alternate-split)
" au Filetype go nmap <leader>gav <Plug>(go-alternate-vertical)
"au FileType go nmap <F9> :GoCoverageToggle -short<cr> " test coverage
" Enable integration with airline.
let g:airline#extensions#ale#enabled = 1

" Javascript
Plug 'pangloss/vim-javascript'
Plug 'mmalecki/vim-node.js'                   " kind of rails.vim for node - gf,gF,etc
Plug 'othree/javascript-libraries-syntax.vim' " syntax highlighting for lodash, react, etc
"Plug 'jelera/vim-javascript-syntax'
"Plug 'othree/yajs.vim' " yet another javascript syntax
"Plug 'vim-scripts/JavaScript-Indent'
"Plug 'kchmck/vim-coffee-script'
Plug 'MaxMEllon/vim-jsx-pretty'
let g:vim_jsx_pretty_colorful_config = 1 " requires vim-javascript
Plug 'jparise/vim-graphql'
au BufNewFile,BufRead *.prisma setfiletype graphql

" typescript
Plug 'leafgarland/typescript-vim'    " syntax
Plug 'HerringtonDarkholme/yats.vim'  " yet another typescript syntax
Plug 'Quramy/vim-js-pretty-template' " template strings coloring
Plug 'jason0x43/vim-js-indent'
" lsp
"Plug 'mhartington/nvim-typescript', {'do': './install.sh'} " LSP plugin
"Plug 'Quramy/tsuquyomi'              " tsserver

" Navigation
"Plug 'kien/ctrlp.vim'
"Plug 'FelikZ/ctrlp-py-matcher'        " Exact filename matches!
Plug 'mileszs/ack.vim'                " :Ack <search> (better; use '\' binding below)
if executable('rg')
  let g:ackprg = 'rg --vimgrep --smart-case --hidden --follow'
endif
"Plug 'Wraul/vim-easytags'
Plug 'jsfaint/gen_tags.vim'
let g:loaded_gentags#gtags=1 " only use ctags (disable gtags)
Plug 'junegunn/fzf.vim' " :FZF, :Files, :BFiles, :BLines, :Lines -- note: only need this, as fzf itself is installed via pkg mgr
Plug 'justinmk/vim-sneak'
"Plug 'goldfeld/vim-seek'              " <leader>s<2 chars>
"Plug 'Lokaltog/vim-easymotion'        " <leader><leader>w
"Plug 'jeetsukumaran/vim-buffergator'  " <leader>b
"Plug 't9md/vim-choosewin'             " -
Plug 'severin-lemaignan/vim-minimap' " sublimetext like mini overview of text in file

" REPL
"Plug 'kassio/neoterm'                 " :T <cmd> - open new or use existing terminal; :TREPLSend; :TREPLSendFile (to e.g. pry, node)
Plug 'metakirby5/codi.vim'            " amazing repl
Plug 'jalvesaq/vimcmdline'            " Send code to repl <leader>i, then Space

" Integrations
Plug 'chrisbra/csv.vim'               " make csvs easier to read and interact with; :CSVTabularize (pretty format)
Plug 'skywind3000/asyncrun.vim'       " used by other plugins to run things asynchronously (or :AsyncRun) Note: not compatible with vim-dispatch as it overrides :make
Plug 'janko-m/vim-test'
Plug 'rizzatti/dash.vim'              " Dash.app integration - :<leader>d / :Dash (word under cursor), :Dash printf, :Dash setTimeout javascript, :DashKeywords backbone underscore javascript
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'              " Gbrowse for fugitive
Plug 'airblade/vim-gitgutter'         " Show status in vim gutter
" Plug 'tpope/vim-git'
" Plug 'tpope/vim-eunuch' " :Mkdir, :SudoWrite, :Chmod, etc
" Plug 'mattn/gist-vim' "github gists
" Plug 'tmux-plugins/vim-tmux'
Plug 'christoomey/vim-tmux-navigator' " or use https://gist.github.com/mislav/5189704#gistcomment-1735600
" Plug 'tbabej/taskwiki'
" Plug 'vimwiki/vimwiki', { 'branch': 'dev' } " required by taskwiki (taskwarrior)
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
Plug 'szw/vim-maximizer' " F3; temporarily maximize a window (or put this in vimrc: https://stackoverflow.com/a/26551079/617320 ) or ':tabe %, which allows you to pop out into a new tab temporarily (unlike CTRL-W T which actually moves the current window out into a new tab). When you’re done, just close the tab.'

" Colors
Plug 'dylanaraps/wal.vim'
Plug 'jonaustin/vim-colorscheme-switcher', { 'branch': 'transparent-bg' } " my fork that keeps transparent bg -- F8/Shift-F8
Plug 'rakr/vim-one' " true color
Plug 'joshdick/onedark.vim'
Plug 'arcticicestudio/nord-vim' " arctic, north-bluish
Plug 'cocopon/iceberg.vim' " Dark blue
Plug 'ajmwagar/vim-deus' " 'A better color scheme for the late night coder'
Plug 'haishanh/night-owl.vim' " 'A 24bit dark Vim colorscheme based on sdras/night-owl-vscode-theme'
Plug 'tyrannicaltoucan/vim-deep-space' " hybrid fork, true color
Plug 'w0ng/vim-hybrid'
Plug 'kristijanhusak/vim-hybrid-material'
Plug 'justinmk/molokai'          " true color fork
Plug 'nanotech/jellybeans.vim'   " true colors
Plug 'lifepillar/vim-solarized8' " true color fork
Plug 'hzchirs/vim-material'      " true colors
Plug 'romainl/flattened'         " solarized 'without the bullshit'
Plug 'dikiaap/minimalist'        " dark material theme
"Plug 'chriskempson/base16-vim'
Plug 'challenger-deep-theme/vim', { 'as': 'challenger-deep' } " true colors
"Plug 'sk1418/last256' " based on hybrid
"Plug 'Lokaltog/vim-distinguished'
""Plug 'jonaustin/vim-colors'
"Plug 'guns/jellyx.vim'
"Plug 'tomasr/molokai'
"Plug 'vim-scripts/xoria256.vim'
"Plug 'ciaranm/inkpot'
"Plug 'jnurmine/Zenburn'
"Plug 'sjl/badwolf' " clojure
"Plug 'morhetz/gruvbox'

" UI
Plug 'reedes/vim-pencil'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
"Plug 'zefei/vim-wintabs'
"Plug 'zefei/vim-wintabs-powerline'

" Misc
"Plug 'wakatime/vim-wakatime'
"Plug 'AD7six/vim-activity-log'
Plug 'fidian/hexmode'

" Embedded neovim
Plug 'glacambre/firenvim', { 'do': { _ -> firenvim#install(0) } }


" Initialize plugin system
call plug#end()
" }}}

" Basics {
set nocompatible
filetype plugin indent on " Automatically detect file types.
let mapleader = ','
let maplocalleader = ','

if has('unix')
  if has('mac') " osx
    set clipboard=unnamed " Copy to the OS clipboard
  else " linux, bsd, etc
    set clipboard=unnamedplus " >=7.3.74 only -- + register -- X11 (ctrl-c/v) clipboard
  endif
endif

set noautochdir " do not automatically change directory
"set cryptmethod=blowfish " strong blowfish encryption (instead of zip) - incomptabile with nvim
" }

" General {
set hidden         " allow unsaved background buffers and remember marks/undo for them
set showtabline=2  " always show tab bar
syntax on          " syntax highlighting
set mouse=a
set signcolumn=yes " Otherwise realtime linter gets annoying
" not every vim is compiled with this, use the following line instead
"autocmd BufEnter * if bufname("") !~ "^\[A-Za-z0-9\]*://" | lcd %:p:h | endif
scriptencoding utf-8
set encoding=utf-8
set autowrite
set shortmess+=filmnrxoOtT " abbrev. of messages (avoids 'hit enter')
set backspace=indent,eol,start " allow backspacing over everything in insert mode

" When you type the first tab, it will complete as much as possible, the second
" tab hit will provide a list, the third and subsequent tabs will cycle through
" completion options so you can complete the file without further keys
set wildmode=longest,list,full
set wildmenu " make tab completion for files,buffers act like bash

set wildignorecase " case insensitive :filename completion

set timeout timeoutlen=1000 ttimeoutlen=100 " Fix slow O inserts
set autoread " If a file is changed outside of vim, automatically reload it without asking

" ONLY disable this when (py)wal colorscheme is used!
"set termguicolors " true colors (colorscheme must have gui colors)

" cursorline only visible in the current window and not in insert mode
autocmd InsertLeave,WinEnter * set cursorline
autocmd InsertEnter,WinLeave * set nocursorline

" underline cursor (terminal as well)
set guicursor=a:hor20-Cursor

" If folding is too slow, possibly add https://github.com/Konfekt/FastFold
set foldmethod=manual
set foldlevelstart=99
nnoremap <silent> <Space> @=(foldlevel('.')?'za':"\<Space>")<CR>
vnoremap <Space> zf

" Prevent Vim from clobbering the scrollback buffer (i.e. don't clear the
" screen on exit). See http://www.shallowsky.com/linux/noaltscreen.html
set t_ti= t_te=

" modelines
set modeline
set modelines=5

" Insert only one space when joining lines that contain sentence-terminating
" punctuation like `.`.
set nojoinspaces

" I don't care what the difference is between c-c and esc in insert mode
imap <c-c> <esc>

" Speed
set lazyredraw     " fix slowdown issues when moving cursor with syntax on
set ttyfast        " assume fast connection (smoother redraw)
set synmaxcol=1024 " Syntax coloring lines that are too long just slows down the world
set nolist         " Hide invisibles

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
"colo vim-material
colo hybrid_material
"colorscheme wal    " change scheme when background changes (pywal)
set incsearch      " find as you type search
set hlsearch       " highlight search terms
set winminheight=0 " windows can be 0 line high
set ignorecase     " case insensitive search
set smartcase      " become temporarily case sensitive when any uppercase letters present in search string
set undofile       " undo even after closing and re-opening a file!
set switchbuf=usetab " If included, jump to the first open window or tab that contains the specified buffer (if there is one).  Otherwise: Do not examine other windows or tabs.


" Formatting {
set wrap         " wrap long lines
set showbreak=↪  " prettier line wrap
set autoindent   " indent at the same level of the previous line
set shiftwidth=2 " use indents of 2 spaces
set expandtab    " tabs should be spaces for sanity

" Key Mappings {

" Aliases {
command! Wq wq
command! WQ wq
command! W w
command! Q q
command! Qa qa

" Switch between 2 files
"nmap <leader><leader> <c-^>
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
cmap w!! w !sudo tee % >/dev/null

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
map <leader>cf :cclose<cr> " close

" remove search highlights
map <silent><leader>. :nohl<cr>

" remove trailing whitespace
nnoremap <leader>W :%s/\s\+$//<cr>:let @/=''<CR>


""" PLUGINS """
" vim-lexical
augroup lexical
  autocmd!
  autocmd FileType markdown,mkd call lexical#init()
  autocmd FileType textile call lexical#init()
  autocmd FileType text call lexical#init() "({ 'spell': 0 })
augroup END
let g:lexical#spell = 0 " 0=disabled, 1=enabled
let g:lexical#thesaurus = ['~/.vim/thesaurus/mthesaur.txt',]

" EasyTag
let g:easytags_async=1 " compile ctags asynchronously
let g:easytags_languages = {
\   'javascript': {
\     'cmd': 'jsctags',
\	    'args': [],
\	    'fileoutput_opt': '-f',
\	    'stdout_opt': '-f-',
\	    'recurse_flag': '-R'
\   }
\}

" Airline/Powerline
let g:airline_powerline_fonts = 1
let g:airline_theme='minimalist'
let g:airline#extensions#tabline#enabled = 1

" vim-test
let g:test#javascript#jasmine#file_pattern = '\v.*/.*spec\.(js|jsx|coffee)$'
let g:test#ruby#rspec#executable = 'bundle exec rspec'
"let g:test#ruby#rspec#executable = 'zeus rspec'
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

"if has('mac') " e.g. 'work'
  let b:ale_fixers = ['prettier', 'eslint'] " ft specific ones below don't work??
  let b:ale_fixers = {'javascript': ['prettier', 'eslint']}
  let b:ale_fixers = {'typescript': ['prettier', 'eslint']}


  let g:ale_fix_on_save = 0
"else
"  let g:ale_linters['javascript'] = ['prettier_standard'] " npm i -g prettier-standard
"  let g:ale_linters['javascript'] = ['']
"  "let g:ale_fixers['javascript'] = ['prettier']
"  let g:ale_fix_on_save = 0 " probably enable this if using prettier-standard
"endif

let g:ale_javascript_prettier_use_local_config = 1 " use local prettier config if available

" less distracting decorations
let g:ale_sign_error = '∙' " Less aggressive than the default '>>'
let g:ale_sign_warning = '◦'
let g:ale_lint_on_enter = 0 " 0=don't lint on file open; 1=do lint on file open
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
nnoremap <silent><leader>/ :CtrlPBuffer<CR>


" Vim-session
let g:session_autoload = 'no'

" Vim-maximizer
nnoremap <silent><F5> :MaximizerToggle<CR>
vnoremap <silent><F5> :MaximizerToggle<CR>
inoremap <silent><F5> <C-o>:MaximizerToggle<CR>

" vimcmdline mappings
let cmdline_map_start          = '<leader>i'
let cmdline_map_send           = '<Space>'
let cmdline_map_send_and_stay  = '<leader><Space>'
"let cmdline_map_source_fun     = '<LocalLeader>f'
"let cmdline_map_send_paragraph = '<LocalLeader>p'
"let cmdline_map_send_block     = '<LocalLeader>b'
"let cmdline_map_quit           = '<LocalLeader>q'

" vimcmdline options
let cmdline_app         = {}
let cmdline_app['ruby'] = 'pry'
let cmdline_vsplit      = 1      " Split the window vertically
let cmdline_esc_term    = 1      " Remap <Esc> to :stopinsert in Neovim's terminal
let cmdline_in_buffer   = 1      " Start the interpreter in a Neovim's terminal
let cmdline_term_height = 15     " Initial height of interpreter window or pane
let cmdline_term_width  = 80     " Initial width of interpreter window or pane
let cmdline_tmp_dir     = '/tmp' " Temporary directory to save files
let cmdline_outhl       = 1      " Syntax highlight the output
let cmdline_auto_scroll = 1      " Keep the cursor at the end of terminal (nvim)

" Ultisnips
"let g:UltiSnipsExpandTrigger="<c-w>"
"let g:UltiSnipsJumpForwardTrigger="<c-b>"
"let g:UltiSnipsJumpBackwardTrigger="<c-x>"
" make YCM compatible with UltiSnips (using supertab)
"let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
"let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
"let g:SuperTabDefaultCompletionType = '<C-n>'

" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"
""" END PLUGINS """


" Omni Completion
"set completeopt+=noinsert " autoselect
"set completeopt=menu,longest,preview
" ^n/^p if popup menu shows up (multiple completions); else just tab
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
" c-y (complete) on enter
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
" make <cr> complete with first completion without selecting a completion
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm() : "\<C-g>u\<CR>"

" File Types {
"" Filetype detection
augroup filetypedetect
  "" Detect .txt as 'text'
  autocmd BufNewFile,BufRead *.txt setfiletype text
  " sass
  autocmd BufNewFile,BufRead *.{sass,scss} setfiletype sass
  "" epub
  autocmd BufReadCmd *.epub call zip#Browse(expand("<amatch>"))
  " markdown
  autocmd BufNewFile,BufRead *.md setfiletype markdown
  autocmd BufNewFile,BufRead *.mkd setfiletype markdown
  autocmd BufNewFile,BufRead *.markdown setfiletype markdown
  " Don't syntax highlight markdown because it's often wrong
  autocmd FileType markdown setlocal syn=off
  " shell
  autocmd BufNewFile,BufRead *.zsh-theme setfiletype zsh
  " javascript
  autocmd BufRead,BufNewFile *.es6 setfiletype javascript
  autocmd BufRead,BufNewFile *.mjs setfiletype javascript
  autocmd FileType javascript setlocal keywordprg='mdn'
  " ruby
  autocmd BufNewFile,BufRead *.feature setfiletype cucumber
  " ruby autocomplete
  "autocmd FileType ruby,eruby let g:rubycomplete_buffer_loading = 1
  "autocmd FileType ruby,eruby let g:rubycomplete_classes_in_global = 1
  "autocmd FileType ruby,eruby let g:rubycomplete_rails = 1
  " json
  autocmd BufRead,BufNewFile .{eslintrc,babelrc} setf json
  " jsonc
  autocmd FileType json syntax match Comment +\/\/.\+$+
  " groovy
  autocmd BufRead,BufNewFile *.gradle setf groovy
  " gitconfig
  autocmd BufRead,BufNewFile gitconfig setf gitconfig
  " arduino
  autocmd BufRead,BufNewFile *.pde set filetype=arduino
  autocmd BufRead,BufNewFile *.ino set filetype=arduino
  " puppet
  autocmd BufRead,BufNewFile *.pp set filetype=puppet
  autocmd FileType js UltiSnipsAddFiletypes puppet
  au FileType puppet set shiftwidth=4
  au FileType puppet set softtabstop=4
  au FileType puppet set tabstop=4
  " eyaml
  autocmd BufRead,BufNewFile *.eyaml set filetype=yaml
  autocmd BufRead,BufNewFile *.yaml set filetype=yaml.cloudformation
augroup END
" }

" Jump to previous line of file after closing and re-opening
" :help last-position-jump
autocmd BufReadPost *
      \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \ exe "normal g`\"" |
      \ endif

" Hacks

" force transparency
hi Normal ctermbg=none guibg=none
hi NonText ctermbg=none guibg=none
hi LineNr ctermbg=none guibg=none
hi CursorLine ctermbg=none guibg=none
hi clear CursorLine
hi clear CursorLineNr
" always use dark grey colorscheme for status line
"autocmd ColorScheme * highlight StatusLine ctermbg=darkgray cterm=NONE guibg=darkgray gui=NONE


" Functions

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

" https://thoughtbot.com/blog/faster-grepping-in-vim
" The Silver Searcher
if executable('rg')
  " Use rg over grep
  set grepprg=rg\ --nogroup\ --nocolor

  " Use rg in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'rg %s -l --nocolor -g ""'

  " rg is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0
endif

" bind K to grep word under cursor
nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>

" This defines a new command Ag to search for the provided text and open a “quickfix” window:
"command -nargs=+ -complete=file -bar Ag silent! grep! <args>|cwindow|redraw!
" bind \ (backward slash) to grep shortcut
nnoremap \ :Ack<SPACE>

" FZF
nnoremap <C-T> :Files<cr>
nnoremap <C-P> :Rg<cr>
nnoremap <C-B> :BLines<cr>

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
" verbose <cmd/func> - debug info
" vim --startuptime /tmp/startup.log +q && vim /tmp/startup.log
" :messages if a message scrolls by too fast (e.g. error on startup)
" C-wL vertical (top/bot) split to horiz (left/right) split (C-wJ to go back)
" c-a / c-x -- increment / decrement number
" delete blank lines -> :g/^$/d
"       or :%s/\n\n/\r/
