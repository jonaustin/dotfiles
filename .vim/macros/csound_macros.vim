" Vim macros file
" Language:	csound	
" Maintainer:	luis jure <luisjure@multitel.com.uy>
" Version:	2.0 
" Last change:	2002 mar 30

" function-key maps
" thanks to hints by Jay Chernick <tropisti@mindspring.com>

" saves present file, compiles and sends to audio output, returns to vim
" (you can try the --sched option if it works in the version you are using)
:map <F9> :w <CR> :!csound -b 4096 -d -o devaudio %:p <CR> <CR>
" saves present file, compiles and writes output to file, returns to vim
:map <F10> :w <CR> :!csound -H1 -W -d -o $SFDIR/test.wav %:p <CR> <CR>
" saves present file, compiles and writes output to file, stays in console (for debugging) 
:map <F11> :w <CR> :!csound -H1 -W -d -o $SFDIR/test.wav %:p <CR>
" plays last compiled file
:map <F12> :!aplay $SFDIR/test.wav <CR><CR>
