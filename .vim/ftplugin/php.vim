function! PHPPreview()
  let l:bufno = bufnr('%')

  new
  set buftype=nofile bufhidden=delete nobuflisted
  setlocal noswapfile

  put =eval('getbufline(' . l:bufno . ', 1, 99999999999)')
  0d
  silent! %!php
endfunction

nnoremap <F12> :call PHPPreview()<CR>

let b:phpgetset_getterTemplate =
  \ "\n" .
  \ "  /**\n" .
  \ "   * @return %varname%\n" .
  \ "   */\n" .
  \ "  public function %funcname%() {\n" .
  \ "    return $this->%varname%;\n" .
  \ "  }"

let b:phpgetset_setterTemplate =
  \ "\n" .
  \ "  /**\n" .
  \ "   * @param %varname%\n" .
  \ "   */\n" .
  \ "  public function %funcname%($%varname%) {\n" .
  \ "    $this->%varname% = $%varname%;\n" .
  \ "  }"

setlocal keywordprg=pman
