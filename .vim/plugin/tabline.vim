if exists("g:loaded_tabline_todd")
  finish
endif
let g:loaded_tabline_todd = 1

function! MyTabLine()
  let s = ''
  for i in range(tabpagenr('$'))
    " select the highlighting
    if i + 1 == tabpagenr()
      let s .= '%#TabLineSel#'
    else
      let s .= '%#TabLine#'
    endif

    " set the tab page number (for mouse clicks)
    let s .= '%' . (i + 1) . 'T'

    " the label is made by MyTabLabel()
    let s .= ' %{MyTabLabel(' . (i + 1) . ')} '
  endfor

  " after the last tab fill with TabLineFill and reset tab page nr
  let s .= '%#TabLineFill#%T'

  " right-align the label to close the current tab page
  if tabpagenr('$') > 1
    let s .= '%=%#TabLine#%999Xclose'
  endif

  return s
endfunction

"Now the MyTabLabel() function is called for each tab page to get its label.

function! MyTabLabel(n)
  " add the tab number
  let label = '['. a:n

  " modified since the last save?
  let buflist = tabpagebuflist(a:n)
  for bufnr in buflist
    if getbufvar(bufnr, '&modified')
      let label .= '+'
      break
    endif
  endfor

  let buffer_name = bufname(buflist[tabpagewinnr(a:n) - 1])
  let label .= ', ' . bufnr(buffer_name)
  let label .= '] '
  let label .= fnamemodify(buffer_name, ':t')

  return label
endfunction

set tabline=%!MyTabLine()

"highlight TabLineFill ctermbg=lightgray
"highlight TabLineSel ctermbg=600 ctermfg=lightgray
