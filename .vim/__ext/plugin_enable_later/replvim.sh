#!/bin/sh

# $Name: R0_3 $
# $Id: replvim.sh,v 1.7 2004/09/28 17:11:43 saul Exp $

[ "X$3" != 'X' ] || { cat << EOUSE ; exit 1 ; }

 Usage : $0 [options] replname code_type file

Runs a screen/vim session on the given file so that one window
holds a repl and another window holds vim. 
Hit:
<leader>e     to eval the current expression 
<leader>r     to evaluate the given marked range
<leader>f     to evaluate the whole file
ctrl-a <TAB>  to switch windows to the repl 
ctrl-a \      to exit everything 
Set the leader in vim with 
:let mapleader = "," 
replacing "," with the key you like the most

Arguments (wrap these in an alias or sh script):
 replname   The repl being used (ocaml, bigloo, chicken have been tested)
 code_type  Can be 'lisp' for lisp-like or 'ocaml' 
 file       The file for vim to edit

Options:
 --lines n  Use n lines of screen space for the repl
 --nopipe   Use files instead of pipes. This can be used when vim 6.3
            gives FSYNC errors when writing to pipes on some OSes.
 --echo     Echo the commands being sent to the repl and allow access
            to them in the readline history

Requires (on your PATH):
  socat (www.dest-unreach.org/socat/)
  screen (http://www.gnu.org/software/screen/)
if you don't have these your life will be much richer for obtaining them

(at saul (dot alien-science org)) welcomes your fixes and extensions
EOUSE

########################################
#### Contains DNA from from VIlisp.vim 
####  By Larry Clapp <vim@theclapp.org>
########################################

##### Options/arguments

repl_lines=10

while [ -n "$1" ]
do
   case $1 in
      --nopipe|--nopipes)
         use_files=1
         ;;
      --echo)
         do_echo=1
         ;;
      --lines)
         shift
         repl_lines=$1
         ;;
      *)
         break
         ;;
   esac
   shift
done

repl=$1
code_type=$2

##### Setup files

temp_dir=/tmp/tmp_for_$$
vim_script=$temp_dir/replvim.vim
pipe=$temp_dir/replvim.pipe
vim_pipe=$temp_dir/replvim.vimpipe
screen_rc=$temp_dir/replscreen.rc
scratch_file=$temp_dir/replvim_scratch

# The vim commands used to yank the beginning and ending
# of a block to eval into vim register 'y'
# The actual vim script functions are defined later
if [ "X$code_type" = 'Xocaml' ] 
then
   find_vim_block='    call REPLWIN_ocaml_block()'
elif [ "X$code_type" = 'Xlisp' ]
then
   find_vim_block='    call REPLWIN_lispy_block()'
fi

# Define the input method based on the repl
if [ "X$repl" = 'Xocaml' ]
then
   repl_in="READLINE,prompt='# '"
elif [ "X$repl" = 'Xcsi' ]
then
   repl_in="READLINE,prompt='#;1> '"
elif [ "X$repl" = 'Xbigloo' ]
then
   repl_in="READLINE,prompt='1:=> '"
fi

### Set up screen arguments for input/output

if [ "X$use_files" = 'X1' ]
then
   # We're using files
   vim_in="OPEN:$vim_pipe,rdonly,ignoreeof"
   vim_feed="OPEN:$vim_pipe,append"
else
   vim_in="PIPE:$vim_pipe,rdonly,ignoreeof"
   vim_feed="PIPE:$vim_pipe,append"
fi

### Start of code
mkdir $temp_dir
touch $vim_pipe
touch $pipe

### Define a vi script to do evaluation

cat << EOVIM > $vim_script
" Run a repl from vim

" ====== Get the current position in the current buffer
function! REPLWIN_get_pos()
  " what buffer are we in?
  let bufname = bufname( "%" )

  " get current position
  let c_cur = virtcol( "." )
  let l_cur = line( "." )
  normal! H
  let l_top = line( "." )

  let pos = bufname . "|" . l_top . "," . l_cur . "," . c_cur

  " go back
  exe "normal! " l_cur . "G" . c_cur . "|"

  return( pos )
endfunction

" ===== Sets the given position in the given buffer
function! REPLWIN_set_pos( pos )
  let mx = '\(\f\+\)|\(\d\+\),\(\d\+\),\(\d\+\)'
  let bufname = substitute( a:pos, mx, '\1', '' )
  let l_top = substitute( a:pos, mx, '\2', '' )
  let l_cur = substitute( a:pos, mx, '\3', '' )
  let c_cur = substitute( a:pos, mx, '\4', '' )

  exe "bu" bufname
  exe "normal! " . l_top . "Gzt" . l_cur . "G" . c_cur . "|"
endfunction

" ===== Get a block of lisp-like data
function! REPLWIN_lispy_block()
  " Behave differently on an opening or closing paren
  exe "normal! \\"lyl"
  if @l == '('
     " Do nothing
  elseif @l == ')'
     exe "normal! %"
  else
     exe "normal! ?(\<cr>"
  endif
  exe "normal! \\"ly%\<cr>"
endfunction

" ===== Get a block of ocaml data
function! REPLWIN_ocaml_block()
  let pos = REPLWIN_get_pos()
  let present = line(".")
  exe "normal! 0\?;;\<cr>"
  let last = line(".")
  if present > last
      exe "normal! \/\\\\w\<cr>\\"ly\/;;\<cr>"
  else
      call REPLWIN_set_pos( pos )
      exe "normal! :0\<cr>\/\\\\w\<cr>\\"ly\/;;\<cr>"
  endif
  let @l = @l . ";;"
endfunction

" ===== Send data to the repl
function! REPLWIN_send( rcmd )
  if a:rcmd == ''
    return
  endif

  let p = REPLWIN_get_pos()
  normal! 

  " goto scratch, delete it, put command, write it to the repl
  silent exe "bu!" g:REPLWIN_scratch
  exe "%d"
  normal! 1G

  " tried append() -- doesn't work the way I need it to
  let old_l = @l
  let @l = a:rcmd
  normal! "lP
  let @l = old_l

  exe 'w >>' s:pipe_name

  call REPLWIN_set_pos( p )
endfunction

" ====== Send the current block to the repl
function! REPLWIN_eval_current()
  let p = REPLWIN_get_pos()
  let old_l = @l
$find_vim_block
  let to_eval = @l
  let @l = old_l
  call REPLWIN_set_pos( p )
  call REPLWIN_send( to_eval )
endfunction

" ======= Send the selected range to the repl
function! REPLWIN_eval_sel() range
  " save position
  let p = REPLWIN_get_pos()

  " yank current visual block
  let old_l = @l
  '<,'> yank l
  let to_eval = @l
  let @l = old_l

  call REPLWIN_set_pos( p )
  call REPLWIN_send( to_eval )
endfunction

" ====== Send the whole file to the repl
function! REPLWIN_eval_file()
  let p = REPLWIN_get_pos()
  let old_l = @l
  exe "normal! \\"ly:0,\$\<cr>"
  let to_eval = @l
  let @l = old_l
  call REPLWIN_set_pos( p )
  call REPLWIN_send( to_eval )
endfunction


" startup stuff
let g:REPLWIN_scratch = '$scratch_file'
let s:pipe_name = '$vim_pipe'

exe "new" g:REPLWIN_scratch
set bufhidden=hide
set nobuflisted
hide

" Mappings
map <leader>e :call REPLWIN_eval_current()<cr>
map <leader>r :call REPLWIN_eval_sel()<cr>
map <leader>f :call REPLWIN_eval_file()<cr>

EOVIM


### Start up screen with a split screen

# screen socat -u $repl_in EXEC:${repl},sigquit
# exec :!. $vim_socat
# vim_socat="socat -u OPEN:$vim_pipe,rdonly,ignoreeof STDOUT"

if  [ "X$do_echo" = 'X1' ]
then

cat <<EOSCR > $screen_rc
split
screen
focus
resize $repl_lines
screen socat -u $repl_in EXEC:${repl},sigquit
title $repl
exec :!. socat -u $vim_in STDOUT
focus
title "vim"
exec ... vim -c 'so $vim_script' $3

EOSCR

else

cat <<EOSCR > $screen_rc
split
screen
focus
resize $repl_lines
screen socat -u $vim_in EXEC:${repl},sigquit
title $repl
exec ... socat -u $repl_in $vim_feed
focus
title "vim"
exec ... vim -c 'so $vim_script' $3

EOSCR

fi


screen -c $screen_rc && rm -rf $temp_dir 

