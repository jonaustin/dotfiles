" Vim filetype plugin file
" Language: php
" Maintainer: kAtremer <katremer@yandex.ru>
" Last Changed: 2005 Jun 14
"
" php_funcinfo.vim
" displays a list of internal PHP functions, their prototypes and descriptions
" intended to be a part of a bigger PHP IDE
"
" to install, put the script and the php_funcinfo.txt file
" to ~/.vim/after/ftplugin (Unix) or $VIM\vimfiles\after\ftplugin (Windows)
"
" default mapping is <F2>, it opens the function list and function description
" window and searches for the word under cursor
"
" <F2> interferes with EasyHTML,
" and EasyHTML is loaded by the default php ftplugin
" that's why the script must go to the /after directory
" but this is PHP, not HTML anyway
" or you may change the mapping
"
" for now, there are some bugs in function list updating
" and no mappings to put the function call back into the text
" and some thoughts on speedup
" quite a lot to improve
" but this is an early version anyway,
" and some other planned plugins are far not ready

" Execute only once {{{

" We don't want to interfere with other plugins
if exists("b:did_ftplugin_ide")
    finish
endif
let b:did_ftplugin_ide=1

" But _this_ script needs to execute only once,
" the same data is used for all calling buffers
if exists("s:did_ftplugin_ide")
    finish
endif
let s:did_ftplugin_ide=1

" }}}
" Set the default compatibility options {{{
let s:save_cpoptions=&cpoptions
set cpoptions&vim
" }}}
let s:funclistfile=expand('<sfile>:p:h').'/php_funcinfo.txt'
" function PHPFuncLookup() opens the function list and searches for the word {{{
function s:PHPFuncLookup()

    " Save options {{{
    let saveignorecase=&ignorecase
    set ignorecase
    let saveiskeyword=&iskeyword
    set iskeyword-=$
    " }}}

    " Get the word to look up {{{
    let searchfor=expand('<cword>')
    if !strlen(searchfor)
        return
    endif
    " }}}

    " Get the function list and description window displayed {{{

    " Can the buffer be lost somewhere? {{{
    if !exists('s:funcbuf')
        let s:funcbuf=bufnr('---\ PHP\ functions\ ---')
    endif
    " }}}

    if s:funcbuf!=-1

        " Buffer is loaded {{{

        if bufwinnr(s:funcbuf)!=-1

            " Move to an existing functions listing window
            execute bufwinnr(s:funcbuf).'wincmd w'

        else

            " Open new window for the hidden buffer {{{
            execute 'silent topleft '.&winwidth.' vsplit ---\ PHP\ functions\ ---'
            execute 'silent belowright new ---\ PHP\ desc\ ---'
            " }}}

            " Move to function list {{{
            execute bufwinnr(s:funcbuf).'wincmd w'
            " }}}

        endif

        " }}}

    else

        " Buffer isn't loaded {{{

        " Create new function listing window {{{
        execute 'silent topleft '.&winwidth.' vsplit '.s:funclistfile
        silent file ---\ PHP\ functions\ ---
        setlocal noswapfile
        setlocal buftype=nowrite
        setlocal bufhidden=hide
        setlocal nonumber
        setlocal nowrap
        iabc <buffer>
        syntax match Normal /^[^@]\+/
        syntax match Ignore /@.*$/
        syntax match Search /^\[.*\]$/
        mapclear <buffer>
        nnoremap <silent> <buffer> <Up>       :call <SID>MoveTo(line('.')-1)<CR>
        nnoremap <silent> <buffer> <Down>     :call <SID>MoveTo(line('.')+1)<CR>
        nnoremap <silent> <buffer> <Left>     :call <SID>MoveTo(line('.')-1)<CR>
        nnoremap <silent> <buffer> <Right>    :call <SID>MoveTo(line('.')+1)<CR>
        nnoremap <silent> <buffer> k          :call <SID>MoveTo(line('.')-1)<CR>
        nnoremap <silent> <buffer> j          :call <SID>MoveTo(line('.')+1)<CR>
        nnoremap <silent> <buffer> h          :call <SID>MoveTo(line('.')-1)<CR>
        nnoremap <silent> <buffer> l          :call <SID>MoveTo(line('.')+1)<CR>
        nnoremap <silent> <buffer> <Home>     :call <SID>MoveTo(1)<CR>
        nnoremap <silent> <buffer> <End>      :call <SID>MoveTo(line('$'))<CR>
        nnoremap <silent> <buffer> <PageUp>   :call <SID>PgUp()<CR>
        nnoremap <silent> <buffer> <PageDown> :call <SID>PgDn()<CR>
        nnoremap <silent> <buffer> <ESC>      :call <SID>Close()<CR>
        nnoremap <silent> <buffer> q          :call <SID>Close()<CR>

        let s:funcbuf=bufnr('---\ PHP\ functions\ ---')

        " Created }}}

        " Create function description window {{{
        execute 'silent belowright new ---\ PHP\ desc\ ---'
        setlocal noswapfile
        setlocal buftype=nowrite
        setlocal bufhidden=hide
        setlocal nonumber
        setlocal linebreak
        iabc <buffer>
        syntax case ignore
        syntax match Function "^\n\@<![a-z_0-9>-]\+\( \[deprecated\]\)\?$" contains=Todo
        syntax match Type "\(???\|void\|number\|callback\|mixed\|string\|int\|boolean\|bool\|flags\|array\|long\|float\|resource\|object\|exception\)" contained
        syntax match Keyword "&" contained
        "syntax match Statement "new" contained
        syntax match Define "new" contained
        syntax match Identifier "\(\(( \|( \[ \|, \| \[, \)\(???\|void\|number\|callback\|mixed\|string\|integer\|int\|long\|float\|real\|double\|boolean\|bool\|flags\|array\|resource\|object\|exception\)[ &]\+\)\@<=[a-z0-9_]\+\|\.\.\.\|???" contained
        syntax match Function "\(^\(???\|void\|number\|callback\|mixed\|string\|int\|boolean\|bool\|flags\|array\|long\|float\|resource\|object\|exception\|new\) \)\@<=[a-z_0-9>-]\+\( (\)\@=" contained
        syntax match FunctionArguments "( [^()]* )" contains=Type,Keyword,Identifier
        syntax match FunctionSynopsis "^\(???\|void\|number\|callback\|mixed\|string\|int\|boolean\|bool\|flags\|array\|long\|float\|resource\|object\|exception\|new\) [a-z_0-9>-]\+ ( [^()]* )$" contains=Type,Define,Function,FunctionArguments
        syntax match Special "^\(\(PHP [345]\( \?>= [0-9.]\+\| \?<= [0-9.]\+\| CVS only\)\?\|[0-9.]\+ - [0-9.]\+ only\)[, ]*\)\+$"
        syntax keyword Todo deprecated
        mapclear <buffer>
        nnoremap <silent> <buffer> <ESC>      :call <SID>Close()<CR>
        nnoremap <silent> <buffer> q          :call <SID>Close()<CR>
        let s:descbuf=bufnr('---\ PHP\ desc\ ---')
        " Created }}}

        " Move to function list {{{
        execute bufwinnr(s:funcbuf).'wincmd w'
        normal gg
        " }}}

        " Force the redraw on MoveTo() {{{
        if exists('s:linebackup')
            unlet s:linebackup
        endif
        " }}}

        " Redraw when entering the window (disabled) {{{
        " does not work properly, invoked on every MoveTo()
        "autocmd BufWinEnter ---\ PHP\ functions\ --- call <SID>MoveTo(line('.'))
        " }}}

        " }}}

    endif

    " }}}

    " Do the search and update the function list {{{
    let oldline=line('.')
    normal gg
    let newline=search('^\( '.searchfor.'@\|\['.searchfor.' *\]$\)', 'W')
    if newline==0
        normal gg
        let newline=search('^[ \[]'.searchfor, 'W')
    endif
    silent execute oldline
    if newline!=0
        call <SID>MoveTo(newline)
    else
        redraw
        echohl ErrorMsg
            echo 'No match for '.searchfor
        echohl None
    endif
    " }}}

    " Restore options {{{
    let &ignorecase=saveignorecase
    let &iskeyword=saveiskeyword
    " }}}
endfunction " }}}
" function s:MoveTo(newline) updates the function list and prints the desc {{{
function s:MoveTo(newline) 

    " Correct the argument {{{
    if a:newline<1
        let newline=1
    elseif a:newline>line('$')
        let newline=line('$')
    else
        let newline=a:newline
    endif
    " }}}

    setlocal modifiable

    " Restore the full line in old place {{{
    if exists('s:linebackup')
        call setline('.', s:linebackup)
    endif
    " }}}

    " Move to the new place and draw the new line {{{
    execute newline
    let s:linebackup=getline('.')
    let s:funcname=strpart(s:linebackup, 1, match(s:linebackup, '@')-1)
    let s:funcdesc=strpart(s:linebackup, match(s:linebackup, '@')+1)
    let modifiedline='['.s:funcname
    let len=strlen(modifiedline)
    while len<winwidth(0)-1
        let modifiedline=modifiedline.' '
        let len=len+1
    endwhile
    let modifiedline=modifiedline.']'
    cal setline('.', modifiedline)
    " }}}

    setlocal nomodifiable

    " Parse the description {{{
    let doespos=match(s:funcdesc, '@')
    let s:funcdoes=strpart(s:funcdesc, 0, doespos)
    let phpverpos=match(s:funcdesc, '@', doespos+1)
    let s:funcphpver=strpart(s:funcdesc, doespos+1, phpverpos-doespos-1)
    let returnpos=match(s:funcdesc, '@', phpverpos+1)
    let s:funcreturns=strpart(s:funcdesc, phpverpos+1, returnpos-phpverpos-1)
    let callpos=match(s:funcdesc, '@', returnpos+1)
    let s:funccall=strpart(s:funcdesc, returnpos+1, callpos-returnpos-1)
    let argstr=''
    let optcount=0
    let argfirst=1
    let argpos=match(s:funcdesc, '@', callpos+1)
    while argpos!=-1
        let argopt=s:funcdesc[argpos-1]+0
        if argopt
            let optcount=optcount+1
        endif
        let argtypepos=match(s:funcdesc, '@', argpos+1)
        let argtype=strpart(s:funcdesc, argpos+1, argtypepos-argpos-1)
        "if !strlen(argtype)
        "   let argtype='???'
        "endif
        let argnamepos=match(s:funcdesc, '@', argtypepos+1)
        let argname=strpart(s:funcdesc, argtypepos+1, argnamepos-argtypepos-1)
        if !strlen(argname)
            let argname='???'
        else
            let argname=substitute(argname, '[^&a-z0-9_.]', '_', 'g')
        endif
        let argstr=argstr
                    \.(argfirst?(argopt?"[ ":''):(argopt?" [, ":', '))
                    \.(strlen(argtype)?argtype.' ':'')
                    \.argname
        let argfirst=0
        let argpos=match(s:funcdesc, '@', argnamepos+1)
    endwhile
    "let s:funcargs=strpart(argstr, 0, strlen(argstr)-2)
    let s:funcargs=argstr
    if optcount>0
        let s:funcargs=s:funcargs.' '
    endif
    while optcount>0
        let s:funcargs=s:funcargs.']'
        let optcount=optcount-1
    endwhile
    if !strlen(s:funcreturns)
        let s:funcreturns='???'
    endif
    let s:desctext=s:funcname
        \.(strlen(s:funccall)?("\n\n".s:funcreturns.' '.s:funccall.' ( '.s:funcargs.' )'):'')
        \.(strlen(s:funcdoes)?("\n\n".s:funcdoes):'')
        \.(strlen(s:funcphpver)?("\n\n".s:funcphpver):'')
    "call confirm(s:desctext)
    " }}}

    " Update the description window {{{
    execute bufwinnr(s:descbuf).'wincmd w'
    setlocal modifiable
    silent %delete
    "put! =s:linebackup
    silent put! =s:desctext
    normal G
    delete
    setlocal nomodifiable
    normal gg
    execute bufwinnr(s:funcbuf).'wincmd w'
    " }}}

endfunction
" }}}
" function s:PgUp() moves the cursor one screen down in the function list {{{
function s:PgUp()
    let oldline=line('.')
    execute "normal \<C-B>"
    let newline=line('.')
    execute oldline
    call <SID>MoveTo(newline)
endfunction
" }}}
" function s:PgDn() moves the cursor one screen up in the function list {{{
function s:PgDn()
    let oldline=line('.')
    execute "normal \<C-F>"
    let newline=line('.')
    execute oldline
    call <SID>MoveTo(newline)
endfunction
" }}}
" s:Close() closes both the function list and the function desc windows {{{
function s:Close()
    if bufwinnr(s:funcbuf)!=-1
        execute bufwinnr(s:funcbuf).'wincmd w'
        silent close
    endif
    if bufwinnr(s:descbuf)!=-1
        execute bufwinnr(s:descbuf).'wincmd w'
        silent close
    endif
endfunction
" }}}
" function s:Cleanup() closes the windows and destroys the buffers {{{
function s:Cleanup()
    call <SID>Close()
    if !exists('s:funcbuf')
        let s:funcbuf=bufnr('---\ PHP\ functions\ ---')
    endif
    if s:funcbuf!=-1
        bdelete s:funcbuf
    endif
    if !exists('s:descbuf')
        let s:descbuf=bufnr('---\ PHP\ desc\ ---')
    endif
    if s:descbuf!=-1
        bdelete s:descbuf
    endif
    unlet s:did_ftplugin_ide
endfunction
" }}}
" set your own mapping here
" Mappings {{{
nmap <silent> <F2> :call <SID>PHPFuncLookup()<CR>
imap <silent> <F2> <ESC><F2>
" }}}
" Undo the stuff we changed {{{
if exists('b:undo_ftplugin')
    "let b:undo_ftplugin="call <SID>Cleanup() | ".b:undo_ftplugin
else
    "let b:undo_ftplugin="call <SID>Cleanup()"
endif
" }}}
" Restore the saved compatibility options {{{
let &cpoptions=s:save_cpoptions
" }}}

" vim:fdm=marker:fmr={{{,}}}

