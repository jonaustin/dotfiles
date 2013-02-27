" Function to allow snippets of other languages inside ruby (like SQL)
function! TextEnableCodeSnip(filetype,start,end,textSnipHl) abort
  let ft=toupper(a:filetype)
  let group='textGroup'.ft
  if exists('b:current_syntax')
    let s:current_syntax=b:current_syntax
    " Remove current syntax definition, as some syntax files (e.g. cpp.vim)
    " do nothing if b:current_syntax is defined.
    unlet b:current_syntax
  endif
  execute 'syntax include @'.group.' syntax/'.a:filetype.'.vim'
  try 
    execute 'syntax include @'.group.' after/syntax/'.a:filetype.'.vim'
  catch
  endtry
  if exists('s:current_syntax')
    let b:current_syntax=s:current_syntax
  else
    unlet b:current_syntax
  endif
  execute 'syntax region textSnip'.ft.'
  \ matchgroup='.a:textSnipHl.'
  \ start="'.a:start.'" end="'.a:end.'"
  \ contains=@'.group
endfunction

call TextEnableCodeSnip('sql', '<<-SQL', 'SQL', 'rubyStringDelimiter')   


"setlocal foldmethod=expr
"setlocal foldexpr=GetRubyFold(v:lnum)
"setlocal foldtext=RubyFoldText()

"function! RubyFoldText()
  "let line_num = NextNonBlankLine(v:foldstart)

  "" Need first non-empty line instead:
  "let desc = getline(v:foldstart)
  "let desc = getline(line_num)

  "let line_count = printf("%2d", v:foldend - v:foldstart + 1)

  "" Setup leading space
  "let lcv = 2
  "let dashes = ''
  "while (lcv < indent(line_num))
    "let dashes = dashes . ' '
    "let lcv += 1
  "endwhile

  "" Craft the perfect descriptor line
  "let sub = substitute(desc, 'do$\|"', '', 'g')
  "let sub = substitute(sub, '^ *', '', 1)

  ""return dashes . '  ' . line_count . '» ' . sub . ' '
  "return dashes . '  ' . line_count . ' ' . sub . ' '
"endfunction

"function! NextNonBlankLine(lnum)
  "let numlines = line('$')
  "let current = a:lnum

  "while current <= numlines
    "if getline(current) =~? '\v\S'
      "return current
    "endif

    "let current += 1
  "endwhile

  "return -2
"endfunction

"function! IndentLevel(lnum)
  "return indent(a:lnum) / &shiftwidth
"endfunction

"function! NextSameIndent(lnum)
  "let numlines = line('$')
  "let current = a:lnum + 1
  "let current_indent = IndentLevel(lnum)

  "while current <= numlines
    "if IndentLevel(current)
      "return current
    "endif

    "let current += 1
  "endwhile

  "return -2
"endfunction

"function! GetRubyFold(lnum)
  ""let &debug = 'msg'

  "" Classes and modules never indent
  "if getline(a:lnum) =~? '\v^\s*(module|class|describe|context|require) '
    "return '0'
  "endif

  "if getline(a:lnum) =~? '\v^\s*(def|it|before|specify) '
    "return '1'
  "endif

  "" Last line triggers folding.
 "if a:lnum == line('$')
    "return '<1'
  "endif

  "if getline(a:lnum+1) =~? '\v^\s*(def |private|protected|it|context|describe).*$'
    "return '<1'
  "endif

  "" -1 means undefined
  "if getline(a:lnum) =~? '\v^\s*$'
    "return '-1'
  "endif

  "return '-1'
"endfunction
