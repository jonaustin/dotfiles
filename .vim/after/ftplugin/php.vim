" Fix matchpairs for PHP (for matchit.vim plugin)
if exists("loaded_matchit")
    let b:match_skip = 's:comment\|string'
    let b:match_words = '<?\(php\)\?:?>,\<switch\>:\<endswitch\>,' .
        \ '\<if\>:\<elseif\>:\<else\>:\<endif\>,' .
        \ '\<while\>:\<endwhile\>,\<do\>:\<while\>,' .
        \ '\<for\>:\<endfor\>,\<foreach\>:\<endforeach\>' .
        \ '<\@<=[ou]l\>[^>]*\%(>\|$\):<\@<=li\>:<\@<=/[ou]l>,' .
        \ '<\@<=dl\>[^>]*\%(>\|$\):<\@<=d[td]\>:<\@<=/dl>,' .
        \ '<\@<=\([^/?][^ \t>]*\)[^>]*\%(>\|$\):<\@<=/\1>,' .
        \ '<:>'
endif

" Uncomment the following function and option if you do not want built-in
" classes and methods appearing in omni completion lists
"
function! MyPHPComplete(findstart, base)
	if !exists('g:php_builtin_functions')
		call phpcomplete#LoadData()
	endif
	let g:php_builtin_object_functions = {}

	let result = phpcomplete#CompletePHP(a:findstart, a:base)
	return result
endfunction
setlocal ofu=MyPHPComplete



" Better indent support for PHP by making it possible to indent HTML sections
" as well.
if exists("b:did_indent")
  finish
endif

" This script pulls in the default indent/php.vim with the :runtime command
" which could re-run this script recursively unless we catch that:
if exists('s:doing_indent_inits')
  finish
endif
let s:doing_indent_inits = 1
runtime! indent/html.vim
unlet b:did_indent
runtime! indent/php.vim
unlet s:doing_indent_inits

function! GetPhpHtmlIndent(lnum)
  if exists('*HtmlIndent')
    let html_ind = HtmlIndent()
  else
    let html_ind = HtmlIndentGet(a:lnum)
  endif
  let php_ind = GetPhpIndent()
  " priority one for php indent script
  if php_ind > -1
    return php_ind
  endif
  if html_ind > -1
    if getline(a:num) =~ "^<?" && (0< searchpair('<?', '', '?>', 'nWb')
          \ || 0 < searchpair('<?', '', '?>', 'nW'))
      return -1
    endif
    return html_ind
  endif
  return -1
endfunction

setlocal indentexpr=GetPhpHtmlIndent(v:lnum)
setlocal indentkeys+=<>>
