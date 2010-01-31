" Vim color file
" Maintainer:   Mingbai <mbbill AT gmail DOT com>
" Last Change:  2006-12-24 20:09:09

set background=dark
if version > 580
    " no guarantees for version 5.8 and below, but this makes it stop
    " complaining
    hi clear
    if exists("syntax_on")
    syntax reset
    endif
endif
let g:colors_name="dasch"

hi Normal   guifg=white guibg=grey17 gui=none

" AlignCtrl default
" AlignCtrl =P0 guifg guibg gui
" Align

" highlight groups
hi Cursor       guifg=black          guibg=yellow   gui=none
hi ErrorMsg     guifg=white          guibg=red      gui=none
hi VertSplit    guifg=gray40         guibg=gray40   gui=none
hi Folded       guifg=grey           guibg=grey30   gui=none
hi FoldColumn   guifg=tan            guibg=grey30   gui=none
hi IncSearch    guifg=#b0ffff        guibg=#2050d0
hi LineNr       guifg=burlywood3     gui=none
hi ModeMsg      guifg=SkyBlue        gui=none
hi MoreMsg      guifg=SeaGreen       gui=none
hi NonText      guifg=cyan           gui=none
hi Question     guifg=springgreen    gui=none
hi Search       guifg=gray80         guibg=#445599  gui=none
hi SpecialKey   guifg=cyan           gui=none
hi StatusLine   guifg=black          guibg=#c2bfa5  gui=bold
hi StatusLineNC guifg=grey           guibg=gray40   gui=none
hi TabLine      guibg=#564d43        guifg=#f7f7f1  gui=underline
hi TabLineFill  guibg=#564d43        guifg=#f7f7f1  gui=underline
hi TabLineSel   guibg=bg             guifg=#f7f7f1  gui=bold
hi Title        guifg=white      gui=none
" hi Visual       guifg=gray17         guibg=tan1     gui=none
hi Visual       guifg=white          guibg=SkyBlue4     gui=none
hi WarningMsg   guifg=salmon         gui=none
" hi Pmenu        guifg=white          guibg=#445599  gui=none
" hi PmenuSel     guifg=#445599        guibg=white
hi Pmenu guibg=#a78869 guifg=#000000 gui=none
hi PmenuSbar guibg=#B99F86 guifg=fg gui=none
hi PmenuSel guibg=#c0aa94 guifg=bg gui=none
hi PmenuThumb guibg=#f7f7f1 guifg=bg gui=none
hi WildMenu     guifg=gray           guibg=gray17   gui=none
hi MatchParen   guifg=cyan           guibg=NONE     gui=bold
hi DiffAdd      guifg=black          guibg=wheat1
hi DiffChange   guifg=black          guibg=skyblue1
hi DiffText     guifg=black          guibg=hotpink1  gui=none
hi DiffDelete   guibg=gray45         guifg=black     gui=none
if has("spell")
    hi SpellBad guisp=#f07070 gui=undercurl
    hi SpellCap guisp=#7070f0 gui=undercurl
    hi SpellLocal guisp=#70f0f0 gui=undercurl
    hi SpellRare guisp=#f070f0 gui=undercurl
endif



" syntax highlighting groups
hi Comment      guifg=#7F7A80     gui=italic
" hi Constant     guifg=salmon         gui=none
hi Constant     guifg=white         gui=none
" hi Identifier   guifg=Skyblue        gui=none
hi Identifier   guifg=#6C9CBD        gui=none
hi Function     guifg=lightgoldenrod2        gui=none
hi Statement    guifg=lightgoldenrod2 gui=none
hi String         guifg=#A4C260
" hi PreProc      guifg=PaleVioletRed2 gui=none
hi PreProc      guifg=#CDA869 gui=none
" hi Type         guifg=tan1           gui=none
hi Type         guifg=white           gui=none
" hi Special      guifg=aquamarine2    gui=none
hi Special      guifg=#A4C260        gui=none
hi Ignore       guifg=grey40         gui=none
hi Todo         guifg=orangered      guibg=yellow2 gui=none

" color terminal definitions
hi SpecialKey   ctermfg=darkgreen
hi NonText      cterm=bold           ctermfg=darkblue
hi Directory    ctermfg=darkcyan
hi ErrorMsg     cterm=bold           ctermfg=7        ctermbg=1
hi IncSearch    cterm=NONE           ctermfg=yellow   ctermbg=green
hi Search       cterm=NONE           ctermfg=grey     ctermbg=blue
hi MoreMsg      ctermfg=darkgreen
hi ModeMsg      cterm=NONE           ctermfg=brown
hi LineNr       ctermfg=3
hi Question     ctermfg=green
hi StatusLine   cterm=bold,reverse
hi StatusLineNC cterm=reverse
hi VertSplit    cterm=reverse
hi Title        ctermfg=5
hi Visual       cterm=reverse
hi VisualNOS    cterm=bold,underline
hi WarningMsg   ctermfg=1
hi WildMenu     ctermfg=0            ctermbg=3
hi Folded       ctermfg=darkgrey     ctermbg=NONE
hi FoldColumn   ctermfg=darkgrey     ctermbg=NONE
hi DiffAdd      ctermbg=4
hi DiffChange   ctermbg=5
hi DiffDelete   cterm=bold           ctermfg=4        ctermbg=6
hi DiffText     cterm=bold           ctermbg=1
hi Comment      ctermfg=darkcyan
hi Constant     ctermfg=brown
hi Special      ctermfg=5
hi Identifier   ctermfg=6
hi Statement    ctermfg=3
hi PreProc      ctermfg=5
hi Type         ctermfg=2
hi Underlined   cterm=underline      ctermfg=5
hi Ignore       ctermfg=darkgrey
hi Error        cterm=bold           ctermfg=7        ctermbg=1


" Ruby
hi rubySymbol         guifg=#6C9CBD


" HTML
hi htmlArg      guifg=lightgoldenrod2
hi htmlEndTag       guifg=lightgoldenrod2
