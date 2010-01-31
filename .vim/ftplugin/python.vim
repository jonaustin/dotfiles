set iskeyword+=.

"
" Python Specific
"

python << EOF
import os
import sys
import vim
for p in sys.path:
   if os.path.isdir(p):
       vim.command(r"set path+=%s" % (p.replace(" ", r"\ ")))
EOF

python
if has("autocmd")
   autocmd FileType python set complete+=k/home/jon/.vim/pydiction isk+=.,(
endif " has("autocmd") 
set tags+=$HOME/.vim/tags/python.ctags
