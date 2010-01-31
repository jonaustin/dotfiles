" Don't use the PHP syntax folding
" setlocal foldmethod=manual
" Turn on PHP fast folds
"EnableFastPHPFolds 

set dictionary-=/home/jon/.vim/ftplugin/php_funclist.txt dictionary+=/home/jon/.vim/ftplugin/php_funclist.txt

" syntax checking for php
" set makeprg=php\ -l\ %
" set errorformat=%m\ in\ %f\ on\ line\ %l

" run file with PHP CLI (CTRL-M)
:autocmd FileType php noremap <C-M> :w!<CR>:!php %<CR>

" PHP parser check (CTRL-L)
:autocmd FileType php noremap <C-L> :!php -l %<CR>

" highlights interpolated variables in sql strings and does sql-syntax highlighting. yay
autocmd FileType php let php_sql_query=1

" does exactly that. highlights html inside of php strings
autocmd FileType php let php_htmlInStrings=1

" discourages use of short tags. deprecated
autocmd FileType php let php_noShortTags=1

" automagically folds functions & methods. this is getting IDE-like isn't it?
autocmd FileType php let php_folding=1

" set auto-highlighting of matching brackets for php only
autocmd FileType php DoMatchParen
autocmd FileType php hi MatchParen ctermbg=blue guibg=lightblue


"nmap <silent> <F4>
"        \ :!ctags-exuberant -f %:p:h/tags
"        \ --langmap="php:+.inc"
"        \ -h ".php.inc" -R --totals=yes
"        \ --tag-relative=yes --PHP-kinds=+cf-v %:p:h<CR>
"set tags=./tags,tags

