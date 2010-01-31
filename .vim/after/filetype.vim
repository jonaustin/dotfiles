"
" Filetype detection
"
augroup filetypedetect
	" Detect .txt as 'text'
    autocmd! BufNewFile,BufRead *.txt setfiletype text
	autocmd! BufNewFile,BufRead *.thtml setfiletype php
	autocmd! BufNewFile,BufRead *.ctp setfiletype php
augroup END
