" Vim syntax file
" Language:	csound	
" Maintainer:	luis jure <luisjure@multitel.com.uy>
" Version:	2.0 
" Last change:	2002 mar 30


" remove any old syntax stuff hanging around
syn clear

" set help program to vim help
set	keywordprg=

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" csound header
syn keyword	csHeader	sr kr ar ksmps nchnls

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" csound opcodes and operators

syn match csOperator	"||"
syn match csOperator	"!="
syn match csOperator	"/"
syn match csOperator	"("
syn match csOperator	")"
syn match csOperator	"&&"
syn match csOperator	"<"
syn match csOperator	"<="
syn match csOperator	"="
syn match csOperator	"=="
syn match csOperator	">"
syn match csOperator	">="
syn match csOperator	"+" 
syn match csOperator	"*" 
syn match csOperator	"-" 

" load list of all opcodes from a file
source	~/.vim/syntax/csound_opcodes.vim

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" csound comments
syn match	csComment	";.*$"
syn region	csComment	start="/\*" end="\*/"

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" csound local and global variables
syn	match	csVariable	"\<[akip].\{-}\>"
syn	match	csVariable	"\<g[aki].\{-}\>"

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" cssound strings
syn match csString        "\".\{-}\""

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" csound defines
syn region csDefine	start="^\s*#\s*\(define\|undef\|include\)\>" skip="\\$" end="$" 

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" csd tags
syn region	csdTags		start="<Cs" end=">"
syn region	csdTags		start="</Cs" end=">"

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" csound instrument
syn match	csInstrument	"instr\s\+\d\+\>"
syn keyword	csInstrument	endin

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" csound label	
syn match	csLabel	"^\s*\<\S\{-}:"	

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" COLOR DEFINITIONS
" I link the Csound classes to some of the default highlighting categories
" defined in synload.vim:
   
hi link	csOpcode	Label
hi link	csOperator	Type
hi link	csHeader	Statement
hi link	csInstrument	Special
hi link	csVariable  	String
hi link	csdTags  	Special
hi link	csComment	Comment
hi link	csDefine	Define
hi link	csLabel 	Define
hi link	csString	String

" to change the appearance you can either:
" 1. link to some other default methods (i. e. Constant, Identifier, etc.) 
" 2. change the color definition of these defaults in synload.vim
" 3. instead of linking to defaults, define your colours right here.
"    For example, you can try the following lines:

" hi csOpcode   	term=bold	ctermfg=darkred 	guifg=red	gui=bold
" hi csInstrument	term=bold	ctermfg=lightblue	guifg=blue	gui=bold
" hi csComment  	term=bold	ctermfg=darkgreen	guifg=#259025	gui=bold
" hi csdTags    	term=bold	ctermbg=blue     	guifg=blue	gui=bold
"
" You can easily change them to suit your preferences.
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let b:current_syntax = "csound"
