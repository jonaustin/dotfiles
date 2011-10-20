""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let b:slime_pry = {"sessionname": "ls", "windowname": "pry"}

function Send_to_Screen(text)
  if !exists("b:slime_pry")
    call Screen_Vars()
  end

  let escaped_text = substitute(shellescape(a:text), "\\\\\n", "\n", "g")
  call system("screen -S " . b:slime_pry["sessionname"] . " -p " . b:slime_pry["windowname"] . " -X stuff " . escaped_text)
endfunction

function Screen_Session_Names(A,L,P)
  return system("screen -ls | awk '/attached/ {print $1}'")
endfunction

function Screen_Vars()
  if !exists("b:slime_pry")
    let b:slime_pry = {"sessionname": "ls", "windowname": "pry"}
  end

  let b:slime_pry["sessionname"] = input("session name: ", b:slime_pry["sessionname"], "custom,Screen_Session_Names")
  let b:slime_pry["windowname"] = input("window name: ", b:slime_pry["windowname"])
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

vmap <C-c><C-c> "ry:call Send_to_Screen(@r)<CR>
nmap <C-c><C-c> vip<C-c><C-c>

nmap <C-c>v :call Screen_Vars()<CR>
