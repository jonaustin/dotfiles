setlocal isfname+=:

set iskeyword+=:


autocmd BufNewFile,BufRead *.p? nnoremap <buffer> <silent> _f :!perldoc -f <cword><cr>

function! PerlDoc()
  normal yy
  let l:this = @
  if match(l:this, '^ *\(use\|require\) ') >= 0
    exe ':new'
    exe ':resize'
    let l:this = substitute(l:this, '^ *\(use\|require\) *', "", "")
    let l:this = substitute(l:this, ";.*", "", "")
    let l:this = substitute(l:this, " .*", "", "")
    exe ':0r!perldoc ' . l:this
    exe ':0'
    return
  endif
  normal yiw
  exe ':new'
  exe ':resize'
  exe ':0r!perldoc -f ' . @
  exe ':0'
endfunction

map <F2> :call PerlDoc()<C-M>

