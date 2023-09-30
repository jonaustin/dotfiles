" Modeline and Notes {
" vim: set sw=2 ts=2 sts=2 et tw=78 foldmarker={,} foldlevel=0 foldmethod=marker ft=vim
"
" }


" Put at beginning so we can group mappings with plugins
let mapleader = ','
let maplocalleader = ','

" vim-plug {{{
" Specify a directory for plugins
" - For Neovim: ~/.local/share/nvim/plugged

" Auto-install
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.config/nvim/bundle')

" Utility
Plug 'skywind3000/asyncrun.vim' " used by other plugins to run things asynchronously (or :AsyncRun) Note: not compatible with vim-dispatch as it overrides :make
Plug 'nvim-lua/plenary.nvim' " lua convenience library

" General Coding
Plug 'majutsushi/tagbar' " :Tagbar
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
lua <<EOD
local treesitter = require('nvim-treesitter.configs')

treesitter.setup {
  highlight = {
    enable = true
  }
}
EOD

" LSP
" Plug 'jose-elias-alvarez/null-ls.nvim'
" lua <<EOD
" require("null-ls").setup({
"     -- note: to disable on the fly - `:lua require("null-ls").disable("<source>")`
"     -- also ALEToggleBuffer
"     -- https://github.com/jose-elias-alvarez/null-ls.nvim/blob/main/doc/BUILTINS.md
"     debug = false,
"     diagnostics_format = "#{m}",
"     fallback_severity = vim.diagnostic.severity.INFO,
"     sources = {
"         -- require("null-ls").builtins.diagnostics.proselint,
"         -- require("null-ls").builtins.diagnostics.write_good,
"         require("null-ls").builtins.diagnostics.ansiblelint,
"         require("null-ls").builtins.diagnostics.cfn_lint,
"         require("null-ls").builtins.diagnostics.checkmake,
"         -- require("null-ls").builtins.diagnostics.cspell, -- ugh, this is the worst: `W: Unknown word` _everywhere_
"         require("null-ls").builtins.diagnostics.eslint,
"         require("null-ls").builtins.diagnostics.flake8,
"         -- require("null-ls").builtins.diagnostics.golangci_lint, -- ugh, takes like 10GB memory and tons of cpu: https://github.com/golangci/golangci-lint/pull/3414
"         require("null-ls").builtins.diagnostics.hadolint,
"         require("null-ls").builtins.diagnostics.luacheck,
"         require("null-ls").builtins.diagnostics.rubocop,
"         require("null-ls").builtins.diagnostics.shellcheck,
"         require("null-ls").builtins.diagnostics.sqlfluff.with({extra_args = { "--dialect", "mysql" }}),
"         require("null-ls").builtins.diagnostics.tfsec,
"         require("null-ls").builtins.diagnostics.yamllint,
"         require("null-ls").builtins.diagnostics.zsh,
"         require("null-ls").builtins.formatting.black,
"         require("null-ls").builtins.formatting.cljstyle,
"         require("null-ls").builtins.formatting.crystal_format,
"         require("null-ls").builtins.formatting.prettier,
"         require("null-ls").builtins.formatting.rubocop,
"         require("null-ls").builtins.formatting.shfmt,
"         require("null-ls").builtins.formatting.stylua,
"         require("null-ls").builtins.formatting.terrafmt,
"         require("null-ls").builtins.formatting.trim_whitespace,
"     },
" })
" EOD
"Plug 'liuchengxu/vista.vim' " LSP viewer/finder :Vista

" Completions
Plug 'github/copilot.vim'
" https://stackoverflow.com/a/22253548/617320
Plug 'neoclide/coc.nvim', {'branch': 'release'}
  " Tips:
  "   :CocRestart If changes aren't available (e.g. editing snippets)

  " FIXME:
  "g:coc_global_extensions = [] " since i constantly forget what ext are in use

  " Use tab for trigger completion with characters ahead and navigate.
  " NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
  " other plugin before putting this into your config.
  inoremap <silent><expr> <CR> coc#pum#visible() ? coc#pum#confirm() : "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"
  inoremap <silent><expr> <C-x><C-z> coc#pum#visible() ? coc#pum#stop() : "\<C-x>\<C-z>"
  " remap for complete to use tab and <cr>
  inoremap <silent><expr> <TAB>
        \ coc#pum#visible() ? coc#pum#next(1):
        \ <SID>check_back_space() ? "\<Tab>" :
        \ coc#refresh()
  inoremap <expr><S-TAB> coc#pum#visible() ? coc#pum#prev(1) : "\<C-h>"
  inoremap <silent><expr> <c-l> coc#refresh()

  hi CocSearch ctermfg=12 guifg=#18A3FF
  hi CocMenuSel ctermbg=109 guibg=#13354A

  " Use `[g` and `]g` to navigate diagnostics
  " Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
  nmap <silent> [g <Plug>(coc-diagnostic-prev)
  nmap <silent> ]g <Plug>(coc-diagnostic-next)

  " GoTo code navigation.
  nmap <silent> gd <Plug>(coc-definition)
  nmap <silent> gy <Plug>(coc-type-definition)
  nmap <silent> gi <Plug>(coc-implementation)
  nmap <silent> gr <Plug>(coc-references)

  " Use K to show documentation in preview window.
  nnoremap <silent> K :call <SID>show_documentation()<CR>
  function! s:show_documentation()
    if (index(['vim','help'], &filetype) >= 0)
      execute 'h '.expand('<cword>')
    elseif (coc#rpc#ready())
      call CocActionAsync('doHover')
    else
      execute '!' . &keywordprg . " " . expand('<cword>')
    endif
  endfunction
  " Highlight the symbol and its references when holding the cursor.
  autocmd CursorHold * silent call CocActionAsync('highlight')
  " Symbol renaming.
  nmap <leader>rn <Plug>(coc-rename)
  " Formatting selected code.
  xmap <leader>f  <Plug>(coc-format-selected)
  nmap <leader>f  <Plug>(coc-format-selected)
  augroup mygroup
    autocmd!
    " Setup formatexpr specified filetype(s).
    autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
    " Update signature help on jump placeholder.
    autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
  augroup end
  " Applying codeAction to the selected region.
  " Example: `<leader>aap` for current paragraph
  xmap <leader>a  <Plug>(coc-codeaction-selected)
  nmap <leader>a  <Plug>(coc-codeaction-selected)
  " Remap keys for applying codeAction to the current buffer.
  nmap <leader>ac  <Plug>(coc-codeaction)
  " Apply AutoFix to problem on the current line.
  nmap <leader>qf  <Plug>(coc-fix-current)
  " Run the Code Lens action on the current line.
  " nmap <leader>cl  <Plug>(coc-codelens-action)
" End CoC
"
Plug 'reedes/vim-lexical' " spelling/dictionary completion
Plug 'SirVer/ultisnips'
  " Ultisnips
  let g:UltiSnipsExpandTrigger = "<c-y>" " mainly so ultisnips doesn't clobber tab; just hit enter or c-y

  " Important: these get overridden by coc.nvim argh
  " let g:UltiSnipsJumpForwardTrigger = "<c-k>"
  " let g:UltiSnipsJumpBackwardTrigger = "<c-j>"
  let g:coc_snippet_next="<tab>"
  let g:coc_snippet_prev="<s-tab>"
Plug 'jonaustin/vim-snippets' " fixme: use custom snippets files with priority>0 to override all built-in snippets rather than forking the repo?
"Plug 'ternjs/tern_for_vim', { 'do': 'npm install' } " also `npm i -g tern` ### Not needed with youcompleteme: https://github.com/Valloric/YouCompleteMe/pull/1849
"Plug 'mhartington/nvim-typescript', { 'do': 'npm install -g typescript' } " or tsuquyomi
"Plug 'roxma/ncm-rct-complete', { 'do': 'gem install rcodetools' }
"Plug 'Shougo/neco-syntax' " syntax completion
Plug 'mattn/emmet-vim' " html helpers e.g. div<c-y>,
" Plug 'jiangmiao/auto-pairs' " auto-close e.g. {} -- fixme: figure out how to make less obnoxious

" General syntax
Plug 'rodjek/vim-puppet'
Plug 'pearofducks/ansible-vim'
Plug 'Glench/Vim-Jinja2-Syntax'
Plug 'earthly/earthly.vim', { 'branch': 'main' }
Plug 'hashivim/vim-terraform'
  let g:terraform_fmt_on_save=1
  au BufNewFile,BufRead *.hcl setfiletype terraform

" Ruby
Plug 'tpope/vim-haml'
Plug 'vim-ruby/vim-ruby'
Plug 'tpope/vim-rake'
Plug 'tpope/vim-rails'
"Plug 'ecomba/vim-ruby-refactoring'

" Golang
Plug 'sebdah/vim-delve'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
  " Notes:
    " ]] and [[ to jump to next/prev function (can also use motions like 3[[
    " and d]])
    " C-] / gd and C-t to jump forward backward in GoDef stack (GoDefStack)
  " https://github.com/fatih/vim-go/issues/3237#issuecomment-853037821

	" To play with more:
	" :GoFillStruct -- `j := play{}` will fill out the struct with default values from play declaration
	" :GoKeyify -- `j2 := play{"me", 42, true} -- expand out the struct with keys
	" :GoReferrers - find all the references to a function, method, type, or variable in your source code.
	" :GoImplements -- list of all interfaces that are implemented
	" :GoFiles -- list of files in the current package
	" :GoMetaLinter -- run all linters
	" :GoTest, :GoFunction, :GoTestCompile, :GoAlternate, :GoTestFunc
	" :GoCoverageToggle, GoCoverageBrowser
	" :GoChannelPeers
	" :TagBar - list entities in file (`?` to see keys)

  " folding
  " needed due to some bug
  set foldmethod=expr
  set foldexpr=nvim_treesitter#foldexpr()
  set nofoldenable

  " https://hackernoon.com/my-neovim-setup-for-go-7f7b6e805876
  "  ]] takes you to the next function or method
  "  [[ takes you to the previous function or method
  "  shift-K to get docs for a func
  let g:go_addtags_transform = "snakecase" " for json struct tags
  au FileType go set noexpandtab
  au FileType go set shiftwidth=2
  au FileType go set softtabstop=2
  au FileType go set tabstop=2
  " note: :GoCoverageClear to clear highlights
  let g:go_auto_type_info = 1 " show actual data type in status line when hovered
  let g:go_highlight_build_constraints = 1
  let g:go_highlight_extra_types = 1
  let g:go_highlight_fields = 1
  let g:go_highlight_functions = 1
  let g:go_highlight_methods = 1
  let g:go_highlight_operators = 1
  let g:go_highlight_structs = 1
  let g:go_highlight_types = 1
  let g:go_metalinter_enabled = ['vet', 'golint', 'errcheck', 'gosec'] " :GoMetaLinter (or GoLint or GoVet)

    " Let coc.nvim handle LSP with :CocInstall coc-go
    let g:go_def_mapping_enabled=0
    let g:go_gopls_enabled=1 " still needed for GoInfo hover (g:go_auto_type_info)

  " coc.nvim integration
    let g:go_diagnostics_level = 0 " let coc handle diagnostics
  " let g:go_metalinter_enabled = []
  let g:go_doc_keywordprg_enabled = 0 " use coc.nvim's much prettier floating window

  "let g:go_auto_sameids = 1 " cursor over one variable will highlight other uses of that var
    " hack to make this less annoying
    hi def goSameId  ctermbg=darkgray cterm=NONE guibg=darkgray gui=NONE

  " mappings
  au FileType go nmap <leader>gt :Tagbar<cr>
  " au FileType go nmap <leader>tb :Tagbar<cr>
	 "    augroup go
    "   autocmd!
    "   autocmd Filetype go
    "     \  command! -bang A call go#alternate#Switch(<bang>0, 'edit')
    "     \| command! -bang AV call go#alternate#Switch(<bang>0, 'vsplit')
    "     \| command! -bang AS call go#alternate#Switch(<bang>0, 'split')
    " augroup END
  au FileType go nmap <leader>ga :GoAlternate<cr>
  au FileType go nmap <leader>gd :GoDeclsDir<cr>
  au FileType go nmap <leader>gr :GoRename
  au FileType go nmap <leader>r :GoRun<cr>
  au FileType go nmap <leader>t :GoTest<cr>
  au FileType go nmap <leader>b :GoBuild<cr>
  au FileType go nmap <leader>c <Plug>(go-coverage-toggle)
  " delve mappings
  au FileType go nmap <leader>bp :DlvToggleBreakpoint<cr>
	" Note: During a test -- have to exec delve explicitly for BPs and such to be triggered:
  au FileType go nmap <leader>dt :DlvTest<cr>
  au FileType go nmap <leader>db :DlvDebug<cr>

  " tracepoint (doesn't stop execution, just prints a note the tracepoint was hit
  au FileType go nmap <leader>tp :DlvToggleTracepoint<cr>
  " switch between file and tests
  au Filetype go nmap <leader>ga <Plug>(go-alternate-edit)
  au Filetype go nmap <leader>gah <Plug>(go-alternate-split)
  au Filetype go nmap <leader>gav <Plug>(go-alternate-vertical)
  au FileType go nmap <F9> :GoCoverageToggle -short<cr> " test coverage

  " highlighting
  hi def goCoverageCovered ctermfg=green guifg=#A6E22E
  hi def goCoverageUncover ctermfg=red guifg=#F92672
  "hi def GoDebugBreakpoint term=standout ctermbg=117 ctermfg=0 guibg=#BAD4F5  guifg=Black
  "hi def GoDebugCurrent term=reverse  ctermbg=12  ctermfg=7 guibg=DarkBlue guifg=White
  hi def GoDebugBreakpoint term=standout ctermbg=117 ctermfg=0 guibg=#BAD4F5  guifg=Black
  hi def GoDebugCurrent term=reverse  ctermbg=12  ctermfg=7 guibg=DarkBlue guifg=White

" ALE
" CoC.nvim integration (must come before ale is loaded)
let g:ale_disable_lsp = 1
" Just use quickfix for everything (for existing key mappings; maybe change back later)
let g:ale_set_loclist = 0
let g:ale_set_quickfix = 1
" Error and warning signs.
let g:ale_sign_error = '⤫'
let g:ale_sign_warning = '⚠'
" Enable integration with airline.
let g:airline#extensions#ale#enabled = 1

" Javascript
Plug 'pangloss/vim-javascript'
"Plug 'othree/javascript-libraries-syntax.vim' " syntax highlighting for lodash, react, etc
""Plug 'jelera/vim-javascript-syntax'
""Plug 'othree/yajs.vim' " yet another javascript syntax
""Plug 'vim-scripts/JavaScript-Indent'
"Plug 'MaxMEllon/vim-jsx-pretty'
"  let g:vim_jsx_pretty_colorful_config = 1 " requires vim-javascript
"Plug 'jparise/vim-graphql'
"  au BufNewFile,BufRead *.prisma setfiletype graphql

" typescript
Plug 'leafgarland/typescript-vim'    " syntax
Plug 'HerringtonDarkholme/yats.vim'  " yet another typescript syntax
Plug 'Quramy/vim-js-pretty-template' " template strings coloring

" Navigation
Plug 'wincent/ferret' " Ack and Acks (multi-file search/replace)
  " First `:Ack something` then Acks to replace `:Acks /something/or other/`
  " (use `dd` in qf window to undo anything you don't want to replace)
  " bind \ (backward slash) to grep shortcut
  nnoremap \ :Ack<SPACE>
          let g:FerretExecutableArguments = {
                                  \   'rg': '--vimgrep --smart-case --hidden --follow'
                                  \ }
"Plug 'Wraul/vim-easytags'
Plug 'jsfaint/gen_tags.vim' " :GenCtags C-]
let g:loaded_gentags#gtags=1 " only use ctags (disable gtags)

" Telescope
if has('nvim')
  Plug 'nvim-telescope/telescope.nvim'
  " Plug 'nvim-lua/popup.nvim'   " api compatible with vim's popup_* (needed by telescope-media-files.nvim)
  Plug 'nvim-telescope/telescope-fzf-native.nvim', { 'do': 'make' } " greatly speeds up telescope
  " Plug 'nvim-telescope/telescope-media-files.nvim' " preview images and such
  " lua require('telescope').extensions.media_files.media_files()
  " fixme: see bottom of vimrc
else
  " FZF
  nnoremap <C-t> :Files<cr>
  nnoremap <C-p> :Rg<cr>
  nnoremap <C-b> :BLines<cr>
  nnoremap <silent><leader>/ :Buffers<CR>
  nnoremap <C-c> :Colors<cr>
  Plug 'junegunn/fzf.vim' " :FZF, :Files, :BLines, :Lines
  Plug 'junegunn/fzf', { 'do': 'yes \| ./install' }
endif

Plug 'justinmk/vim-sneak'              " <leader>s<2 chars>
"Plug 'goldfeld/vim-seek'              " <leader>s<2 chars>
"Plug 'Lokaltog/vim-easymotion'        " <leader><leader>w
"Plug 'jeetsukumaran/vim-buffergator'  " <leader>b
"Plug 't9md/vim-choosewin'             " -
"Plug 'severin-lemaignan/vim-minimap' " sublimetext like mini overview of text in file

" REPL
"Plug 'kassio/neoterm'                 " :T <cmd> - open new or use existing terminal; :TREPLSend; :TREPLSendFile (to e.g. pry, node)
"Plug 'metakirby5/codi.vim'            " amazing repl
"Plug 'jalvesaq/vimcmdline'            " Send code to repl <leader>i, then Space

" Integrations
Plug 'ptzz/lf.vim' " note: should come before floaterm
  let g:lf_map_keys = 0
  map <leader>lf :Lf<cr>
Plug 'voldikss/vim-floaterm'
  let g:floaterm_position = 'bottom'
  let g:floaterm_width = 0.98
  let g:floaterm_autoclose = 2
  let g:floaterm_height = 0.4
  let g:floaterm_keymap_toggle = '<leader>ft'
  nnoremap <C-c><C-s> :FloatermSend<CR>
  vnoremap <C-c><C-s> :FloatermSend<CR>
"Plug 'chrisbra/csv.vim'               " make csvs easier to read and interact with; :CSVTabularize (pretty format)
"Plug 'janko-m/vim-test'
"Plug 'rizzatti/dash.vim'              " Dash.app integration - :<leader>d / :Dash (word under cursor), :Dash printf, :Dash setTimeout javascript, :DashKeywords backbone underscore javascript
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
Plug 'rhysd/git-messenger.vim' " show commit message in floating window :GitMessenger / <leader>gm

" Commands
if has('nvim')
  " FIXME: figure out why my lua runtimepaths are b0rked (I had to manually symlink Comment.nvim/lua/Comment -> ~/.config/nvim/lua/Comment)
  Plug 'numToStr/Comment.nvim'
  lua require('Comment').setup()

  " chatgpt
  Plug 'muniftanjim/nui.nvim'
  Plug 'jackmort/chatgpt.nvim'
  lua require('chatgpt').setup()
else
  Plug 'scrooloose/nerdcommenter'
endif
Plug 'mbbill/undotree'
"Plug 'sjl/gundo.vim'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'junegunn/vim-easy-align' " :EasyAlign /<regex>/
Plug 'w0rp/ale' " asynchronous linter
Plug 'xolox/vim-session' " e.g. :OpenSession :SaveSession
  let g:session_autosave = 'no'
  Plug 'xolox/vim-misc' " required by vim-session
Plug 'szw/vim-maximizer' " F3; temporarily maximize a window (or put this in vimrc: https://stackoverflow.com/a/26551079/617320 ) or ':tabe %, which allows you to pop out into a new tab temporarily (unlike CTRL-W T which actually moves the current window out into a new tab). When you’re done, just close the tab.'

" Colors
" Plug 'tribela/vim-transparent' " force transparency.. :TransparentToggle
Plug 'folke/tokyonight.nvim'
lua << EOD
require("tokyonight").setup({
  style = "storm", -- The theme comes in three styles, `storm`, `moon`, a darker variant `night` and `day`
  transparent = true, -- Enable this to disable setting the background color
})
EOD
" Plug 'romainl/Apprentice'
" TODO: switch to treesitter and use colorscheme compatible with TS
"Plug 'pgdouyon/vim-ying-yang' " black white (yin)
" Plug 'dylanaraps/wal.vim'
"Plug 'jonaustin/vim-colorscheme-switcher', { 'branch': 'transparent-bg' } " my fork that keeps transparent bg -- F8/Shift-F8
" Plug 'arcticicestudio/nord-vim' " arctic, north-bluish
" Plug 'cocopon/iceberg.vim' " Dark blue
" Plug 'tyrannicaltoucan/vim-deep-space' " hybrid fork, true color
" Plug 'w0ng/vim-hybrid'
"Plug 'kristijanhusak/vim-hybrid-material'
" Plug 'jonaustin/vim-hybrid-material'
" Plug 'justinmk/molokai'          " true color fork
" Plug 'nanotech/jellybeans.vim'   " true colors
" Plug 'lifepillar/vim-solarized8' " true color fork
" Plug 'hzchirs/vim-material'      " true colors
" Plug 'romainl/flattened'         " solarized 'without the bullshit'
" Plug 'dikiaap/minimalist'        " dark material theme
"Plug 'chriskempson/base16-vim'
" Plug 'challenger-deep-theme/vim', { 'as': 'challenger-deep' } " true colors
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
Plug 'stevearc/dressing.nvim' " subtle ui improvements
Plug 'shortcuts/no-neck-pain.nvim', { 'tag': '*' } " :NoNeckPain (center window)
Plug 'APZelos/blamer.nvim' " codelens for vim
  let g:blamer_show_in_visual_modes = 0
  nnoremap <leader>cl :BlamerToggle<cr>
"   let g:blamer_enabled = 1
"   highlight Blamer guifg=lightgrey
" Plug 'folke/which-key.nvim'  " show keypresses and options for discovery and presentations
  " lua require("which-key").setup {}
Plug 'romainl/vim-cool' " disable highlighting after search
" Plug 'reedes/vim-pencil' " focused writing :Pencil
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
"Plug 'zefei/vim-wintabs'
"Plug 'zefei/vim-wintabs-powerline'

" Misc
"Plug 'wakatime/vim-wakatime'
"Plug 'AD7six/vim-activity-log'
" Plug 'fidian/hexmode'

" Embedded neovim in firefox
Plug 'glacambre/firenvim', { 'do': { _ -> firenvim#install(0) } }
if exists('g:started_by_firenvim')
  set guifont=Inconsolata:h12
  set laststatus=0

  let g:firenvim_config = {
        \ 'localSettings': {
        \ '.*': {
        \ 'takeover': 'never',
        \ },
        \ }
        \ }
endif


" Initialize plugin system
call plug#end()
" }}}

" Basics {
set nocompatible
filetype plugin indent on " Automatically detect file types.

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
" not every vim is compiled with this, use the following line instead
"autocmd BufEnter * if bufname("") !~ "^\[A-Za-z0-9\]*://" | lcd %:p:h | endif
scriptencoding utf-8
set encoding=utf-8
set autowrite                  " save the file when :make is called (and related like vim-go/GoBuild)
set backspace=indent,eol,start " allow backspacing over everything in insert mode

set shortmess+=filmnrxoOtT " abbrev. of messages (avoids 'hit enter')
" Don't pass messages to |ins-completion-menu|.
" https://github.com/neoclide/coc.nvim/#example-vim-configuration
set shortmess+=c

" Always show the signcolumn, otherwise it would shift the text each time diagnostics appear/become resolved.
" Also do not overwrite the number column as that just gets confusing
set signcolumn=yes

" trick to show relative, but with the current line showing the current line number (instead of 0)
" note: use `:set nornu` to see only absolute
set number relativenumber

" Having longer updatetime (default is 4000 ms) leads to noticeable delays and poor user experience
set updatetime=300

" colon/command completion
" When you type the first tab, it will complete as much as possible, the second
" tab hit will provide a list, the third and subsequent tabs will cycle through
" completion options so you can complete the file without further keys
set wildmode=longest,list,full
set wildchar=<Tab>
set wildmenu       " provide navigable list of suggestions (tab, C-n, right; S-tab, C-p, left)
set wildignorecase " case insensitive :filename completion

set timeout timeoutlen=1000 ttimeoutlen=100 " Fix slow O inserts
set autoread " If a file is changed outside of vim, automatically reload it without asking

" only disable this for (py)wal colorscheme or one that doesn't support 24-bit
if (has("termguicolors"))
  set termguicolors " true colors (colorscheme must have gui colors)
endif

" cursorline only visible in the current window and not in insert mode
autocmd InsertLeave,WinEnter * set cursorline
autocmd InsertEnter,WinLeave * set nocursorline

" underline cursor (terminal as well)
set guicursor=a:hor20-Cursor

" note: let treesitter handle folding
" nnoremap <silent> <Space> @=(foldlevel('.')?'za':"\<Space>")<CR>
" vnoremap <Space> zf

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
set backup " backups are nice
set backupdir=$HOME/.vimbackup " but not when they clog .
set directory=$HOME/.vimswap " Same for swap files
set viewdir=$HOME/.vimviews " same but for view files
set undodir=$HOME/.vimundo " ditto

" Creating directories if they don't exist
silent execute '!mkdir -p $HOME/.vimbackup'
silent execute '!mkdir -p $HOME/.vimswap'
silent execute '!mkdir -p $HOME/.vimviews'
silent execute '!mkdir -p $HOME/.vimundo'

" Vim UI {
set background=dark                 " Assume a dark background
colo tokyonight
"colo vim-material
"colo hybrid_material
  " let g:hybrid_transparent_background = 1
" colo nord
" colo apprentice
"colorscheme wal    " change scheme when background changes (pywal)
set incsearch      " find as you type search
set hlsearch       " highlight search terms
set winminheight=0 " windows can be 0 line high
set ignorecase     " case insensitive search
set smartcase      " become temporarily case sensitive when any uppercase letters present in search string
set undofile       " undo even after closing and re-opening a file!
set switchbuf=usetab " If included, jump to the first open window or tab that contains the specified buffer (if there is one).  Otherwise: Do not examine other windows or tabs.

" Niceties
"" Keep things vertically centered
" note: could always just use scrolloff
  " Center screen on next/previous selection.
  nnoremap n nzz
  nnoremap N Nzz
  " Last and next jump should center too.
  nnoremap <C-o> <C-o>zz
  nnoremap <C-i> <C-i>zz
  nnoremap <C-d> <C-d>zz
  nnoremap <C-u> <C-u>zz

" Formatting {
set wrap         " wrap long lines
set autoindent   " indent at the same level of the previous line
set shiftwidth=2 " use indents of 2 spaces
set expandtab    " tabs should be spaces for sanity

" Key Mappings {

" Typo aliases {
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
map <leader>qw  :wq<cr>

" quickfix
map <leader>qf :cope<cr> " open
map <leader>cf :cclose<cr> " close
map <C-n> :cnext<CR>
map <C-m> :cprevious<CR>

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
let g:lexical#spell = 1 " 0=disabled, 1=enabled
let g:lexical#thesaurus = ['~/.config/nvim/thesaurus/mthesaur.txt',]

" EasyTag
" let g:easytags_async=1 " compile ctags asynchronously
" let g:easytags_languages = {
" \   'javascript': {
" \     'cmd': 'jsctags',
" \	    'args': [],
" \	    'fileoutput_opt': '-f',
" \	    'stdout_opt': '-f-',
" \	    'recurse_flag': '-R'
" \   }
" \}

" Airline/Powerline
let g:airline_powerline_fonts = 1
let g:airline_theme='tokyonight'
let g:airline#extensions#tabline#enabled = 1

" vim-test
" let g:test#javascript#jasmine#file_pattern = '\v.*/.*spec\.(js|jsx|coffee)$'
" let g:test#ruby#rspec#executable = 'bundle exec rspec'
" "let g:test#ruby#rspec#executable = 'zeus rspec'
" let test#strategy = 'neovim' "'neoterm'
" nmap <silent> <leader>t :TestNearest<CR>
" nmap <silent> <leader>T :TestFile<CR>
" "nmap <silent> <leader>a :TestSuite<CR>
" nmap <silent> <leader>l :TestLast<CR>
" nmap <silent> <leader>g :TestVisit<CR>

" ALE asynchronous linter
" clear all fixers and linters
let g:ale_fixers = {}
let g:ale_linters = {
			\ 'go': ['gopls'],
			\}

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
let g:ale_enabled = 0
highlight clear ALEErrorSign
highlight clear ALEWarningSign

" Codi repl
let g:codi#rightalign=0
let g:codi#width=80

" NERDCommenter
" let g:NERDDefaultAlign = 'left' " put comments at col 0

" NERDTree
map <S-q> :NERDTreeToggle<cr>

" Vim-session
let g:session_autoload = 'no'

" " vimcmdline mappings
" let cmdline_map_start          = '<leader>i'
" let cmdline_map_send           = '<Space>'
" let cmdline_map_send_and_stay  = '<leader><Space>'
" "let cmdline_map_source_fun     = '<LocalLeader>f'
" "let cmdline_map_send_paragraph = '<LocalLeader>p'
" "let cmdline_map_send_block     = '<LocalLeader>b'
" "let cmdline_map_quit           = '<LocalLeader>q'
"
" " vimcmdline options
" let cmdline_app         = {}
" let cmdline_app['ruby'] = 'pry'
" let cmdline_vsplit      = 1      " Split the window vertically
" let cmdline_esc_term    = 1      " Remap <Esc> to :stopinsert in Neovim's terminal
" let cmdline_in_buffer   = 1      " Start the interpreter in a Neovim's terminal
" let cmdline_term_height = 15     " Initial height of interpreter window or pane
" let cmdline_term_width  = 80     " Initial width of interpreter window or pane
" let cmdline_tmp_dir     = '/tmp' " Temporary directory to save files
" let cmdline_outhl       = 1      " Syntax highlight the output
" let cmdline_auto_scroll = 1      " Keep the cursor at the end of terminal (nvim)

""" END PLUGINS """


" Omni Completion
"set completeopt+=noinsert " autoselect
"set completeopt=menu,longest,preview
" ^n/^p if popup menu shows up (multiple completions); else just tab
" inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
" inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
" " c-y (complete) on enter
" inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

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
  "au FileType puppet set shiftwidth=4
  "au FileType puppet set softtabstop=4
  "au FileType puppet set tabstop=4
  " eyaml
  autocmd BufRead,BufNewFile *.eyaml set filetype=yaml
  autocmd BufRead,BufNewFile *.yaml set filetype=yaml.cloudformation

  " wrap quickfix window
  autocmd FileType qf setlocal wrap
augroup END

" }

" Jump to previous line of file after closing and re-opening
" :help last-position-jump
autocmd BufReadPost *
      \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \ exe "normal g`\"" |
      \ endif

" Hacks


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
if executable('rg')
  " Use rg over grep
  set grepprg=rg\ --nogroup\ --nocolor
endif

" bind K to grep word under cursor
"nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR> (fixme: don't use K; that's for viewing keyword docs

" Fix window switching for terminal
" https://www.reddit.com/r/neovim/comments/9sm1bp/how_to_switch_between_windows_in_terminal_mode/
" vim bug: this doesn't work for some reason when switching TO a floating window;
"     have to use the toggle floaterm command for that (e.g. ',ft')
"     https://github.com/voldikss/vim-floaterm/issues/134
"         "You can not because neovim/vim doesn't support wincmd h/j/k/l for floating windows."
if has('nvim')
  augroup vimrc_term
    autocmd!
    autocmd WinEnter term://* nohlsearch
    autocmd WinEnter term://* startinsert

    autocmd TermOpen * tnoremap <buffer> <C-h> <C-\><C-n><C-w>h
    autocmd TermOpen * tnoremap <buffer> <C-j> <C-\><C-n><C-w>j
    autocmd TermOpen * tnoremap <buffer> <C-k> <C-\><C-n><C-w>k
    autocmd TermOpen * tnoremap <buffer> <C-l> <C-\><C-n><C-w>l
    autocmd TermOpen * tnoremap <buffer> <Esc> <C-\><C-n>
  augroup END
endif

" " using https://github.com/junegunn/fzf.vim & fzf installed.
" augroup vimrc_term_fzf
"   autocmd!
"   " Do some other stuff independent of nvim.
"   if has('nvim')
"     autocmd FileType fzf tunmap <buffer> <Esc>
"     autocmd FileType fzf tunmap <buffer> <C-h>
"     autocmd FileType fzf tunmap <buffer> <C-j>
"     autocmd FileType fzf tunmap <buffer> <C-k>
"     autocmd FileType fzf tunmap <buffer> <C-l>
"   endif
" augroup END

" show what highlight group is used for highlight under cursor
nmap <leader>hl :call <SID>SynStack()<CR>
function! <SID>SynStack()
  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc

set eol " for some reason this is getting turned off for k8s yamls

" arrrrgh fixme; can't have these right under telescope as they don't work;
" but they don't seem to be getting overwritten either...wtf.
if has('nvim')
  nnoremap <C-t> <cmd>Telescope find_files<cr>
  nnoremap <C-p> <cmd>Telescope live_grep<cr>
  nnoremap <silent><leader>/ <cmd>Telescope buffers<cr>
  nnoremap <leader>fh <cmd>Telescope help_tags<cr>
  nnoremap <leader>fc <cmd>Telescope colorscheme<cr>
endif

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
" delete multiple blank lines: :%!cat -s
" verbose Xmap <leader>c # show imap/nmap/map/etc for <leader>c or whatnot
" show value of set var with e.g. `set modeline?`; let is just the var `let g:plugin_var`
" :enew|pu=execute('<colon command>') " copy the output of any :colon command to a new buffer
" zz/. - center current liner horizontally on the screen (z -/+ or b/t to put current line at bottom/top)
