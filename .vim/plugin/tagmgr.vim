" Tag Manager Plugin <tagmgr.vim>
"
"
" Script Info and Documentation 
"=============================================================================
"    Copyright: Copyright (C) November 2008, Hari Rangarajan
"               Permission is hereby granted to use and distribute this code,
"               with or without modifications, provided that this copyright
"               notice is copied with it. Like anything else that's free,
"               tagmgr.vim is provided *as is* and comes with no
"               warranty of any kind, either expressed or implied. In no
"               event will the copyright holder be liable for any damamges
"               resulting from the use of this software.
"
" Name Of File: tagmgr.vim
"  Description: Tag Manager Vim Plugin
"   Maintainer: Hari Rangarajan <hari.rangarajan@gmail.com>
"          URL: http://vim.sourceforge.net/scripts/script.php?script_id=
"  Last Change: November 3, 2008
"      Version: 0.02 (Beta Pre-release)
"
"=============================================================================
"
"
"  Description:
"       Plugin provides an integrated environment within Vim for building, maintaining,
"       and using tag databases.
"
"       Features:
"           * Specify a list of files or regex file search patterns for inclusion
"             in tags database
"           * Support for multiple databases
"           * Build and track tag databases for freshness
"               + Tracks new file creations when using regex file patterns                 
"               + Update tag databases on every file save with
"                 efficient incremental updates
"               + Tracks file changes outside of Vim
"
"       Limitations:
"           * Might slow-up with huge databases because of  Vim scripting
"             speed and API limitations
"               - Recommended method is to split multiple tag databases for 
"                 each sub-directory
"       
"
"  Requirements: 
"                1) Exuberant Ctags or other equivalent tag generation app
"                2) Vim 7 and above
"
"  Installation: 
"               Copy this file to ~/.vim/plugins/
"               or to /vimfiles/plugins/  (on Win32 platforms) 
"
"  QuickStart:
"          1) Add the following to one of the following files:
"               (a) ~/.gvimrc, or 
"               (b)  ~/.vim/after/plugin/tagmgr.vim  (filename has to be the same!)
"                   Windows: $VIM\vimfiles\after\plugin\tagmgr.vim
"
"              NOTE: Adding to .vimrc will *NOT* work. Only GVim will use
"                    .gvimrc file for startup. If you use console Vim, then
"                    use the 'after' option.
"
"              TagManagerAddDb tags *.[ch]  <change as per source language )
"              (or)
"              TagMangerAddDb tags **/*.[ch]  <Recursive; use only if there are
"              directories> 

"
"          2. Launch vim from the project root directory.
"
"          NOTE: The commands can be run from within Vim on the project root
"                directory without using the start-up scripts.
"
"  Usage:
"
"       Commands:
"        :TagManagerAddDb  <tagDB name> <filelist/pattern> [directory]
"           Illustration:
"           :TagManagerAddDb tags *.[ch] 
"           :TagManagerAddDb tags *.[ch] .
"           [Note that both commands do the same, the directory is optional;
"           if omitted, the current directory is used]
"
"           :TagManagerAddDb tags **/*.[ch] 
"             Recursively add all .c and .h files in the current directory and
"             directories under it. More complicated search patterns are
"             possible with the "**" operator. For more help, please see :help **
"
"           :TagManagerAddDb tags **/*.[ch] ./dir1/,./dir2/,./dir3/
"             Selectively build individual tags in each of these directories
"             recusively. Paths are relative to current directory. It is also 
"             possible to specify the absolute paths
"
"           The sequence of commands has the same effect as the previous
"           command
"           :TagManagerAddDb tags **/*.[ch] ./dir1
"           :TagManagerAddDb tags **/*.[ch] ./dir2
"           :TagManagerAddDb tags **/*.[ch] ./dir3
"           :TagManagerAddDb tags  */*.[ch] ./dir4    [not recursive]
"
"
"           Using file lists
"           :TagManagerAddDb tags files.lst dir1
"               Uses the "files.lst" file in directory "dir1" for the database
"               creation. Instead of using regex patterns, file lists can also
"               specified. Note that these files must be created manually
"               before-hand.
"                       
"        :TagManagerShowDbs
"           Shows the current loaded Dbs and their internal index
"
"        :TagManagerRemoveDb <index>
"            Use it to remove already loaded Tag Dbs. Index must be
"            obtained from the show command
"
"        Debugging:
"
"           :TagManagerShowDbDetail <index>
"               Show more details of the database: i.e., list of files tracked
"                  
"           :TagManagerShowLog
"               Internal debug log; displays the sequence of
"               actions/events/system commands executed. In case of anomalous
"               behavior, please log this.
"
"        Configuration:      
"            :let g:TagMgr_CtagsProgName = 'ctags'
"               Specify the executable for the tag generation application.
"               
"            :let g:TagMgr_CtagsProgDir = ''
"               Specify the directory in which the ctags app can be found. If
"               it is on the path, then it can be left empty.
"                   
"            :let g:TagMgr_CtagsExtraOpts = ''
"               Extra options for building the database. A better option is to
"               use the ~/.ctagsrc file to specify extra options; this will be
"               automatically used for every ctags run.
"
"            :let g:TagMgr_AutoTrackDbs = 1
"               Disable (0) or enable (1). TagManager will automatically setup
"               the Vim 'tags' variable to reflect the current paths. (More to
"               come later for this feature)
"
"            :let g:TagMgr_LogMaxLines = 30
"               Internal debug log. Sets the limit for the number of lines.
"
"            :let g:TagMgr_SourceListFile = 'files.lst'
"               When using a regex for file specification, TagManager will
"               create a file list with this name and then pass it to ctags.
"
"
if !exists('loaded_tagmgr') && v:version >= 700
  " First time loading the tagmgr plugin
  let loaded_tagmgr = 1
else
   finish 
endif

" Line continuation used here
let s:cpo_save = &cpoptions
set cpoptions&vim


if !exists("g:TagMgr_CtagsProgDir")
    let g:TagMgr_CtagsProgDir = ''
endif

if !exists("g:TagMgr_CtagsProgName")
    let g:TagMgr_CtagsProgName = 'ctags'
endif

if !exists("g:TagMgr_CtagsExtraOpts")
    let g:TagMgr_CtagsExtraOpts = ''
endif

if !exists("g:TagMgr_LogMaxLines")
    let g:TagMgr_LogMaxLines = 30
endif

if !exists("g:TagMgr_SourceListFile")
    let g:TagMgr_SourceListFile = 'files.lst'
endif

if !exists("g:TagMgr_AutoTrackDbs")
    let g:TagMgr_AutoTrackDbs = 1
endif

"=================================================================
" Code
"=================================================================

let s:dbg_level = 0
let s:pluginname = 'TagManager'
let s:miscbucket = '#'
let s:statusLog = []
let s:TagMgr_FileList = []
let s:autotrack = 0
let s:TagMgr_Init = 0

function! TagMgr_LogAddMsg(msg)
   if len(s:statusLog) > g:TagMgr_LogMaxLines
    call remove(s:statusLog, 0)
   endif

   call add(s:statusLog, localtime().": ". a:msg)
endfunction

function! TagMgr_LogShowMsgs ()
    for alogline in s:statusLog
        echo alogline
    endfor
endfunction


function! TagMgr_GenerateTags(dstdir, srcfile, tagdb, sort, islst)
    let cmdstr = 'cd '. a:dstdir . " && "

    let cmdstr .= g:TagMgr_CtagsProgDir
    let cmdstr .= g:TagMgr_CtagsProgName
    
    if a:sort ==0 
        let cmdstr .= " -u "
    endif

    let cmdstr .=  " ".g:TagMgr_CtagsExtraOpts
    let cmdstr .= " -f".a:tagdb

    if a:islst == 1
        let cmdstr .= " -L".a:srcfile
    else
        let cmdstr .= " ".a:srcfile
    endif

    call TagMgr_LogAddMsg("Executing system command ". cmdstr)
    return system(cmdstr)
endfunction


function! TagMgr_InitBucket(buckets, key)
        let a:buckets[a:key] = {}
        let a:buckets[a:key].min = 99999999
        let a:buckets[a:key].max = -1
        let a:buckets[a:key].mods = []
endfunction


function! TagMgr_CreateAlphaHashBucket ()
    let buckets = {}
    let charlist = [char2nr(s:miscbucket)] + 
                    \ range(char2nr('a'), char2nr('z')) +
                    \ range(char2nr('A'), char2nr('Z'))

    for achar in charlist
        call TagMgr_InitBucket(buckets, nr2char(achar))
    endfor

    return buckets
endfunction


function! TagMgr_BuildHashAndAlphaFromTags(taglst)
    let taghash = {}
    let idx = 0
    let alphahash = TagMgr_CreateAlphaHashBucket()
    for val in a:taglst
        call TagMgr_InitHashEntryFromVal(taghash, val, idx)
        call TagMgr_InitAlphaHashBucket(alphahash, val, idx)
        let idx += 1
    endfor
    return [taghash, alphahash]
endfunction


function! TagMgr_BuildAlphaHash(taglst)
    let idx = 0
    let alphahash = TagMgr_CreateAlphaHashBucket()

    for val in a:taglst
        call TagMgr_InitAlphaHashBucket(alphahash, val, idx)
        let idx += 1
    endfor
    return alphahash
endfunction


function! TagMgr_ExtractKeyFromTag(val)
    let tag_keys = matchlist(a:val, '\(\w\+\)\t\(.\{-}\)\t')
    return tag_keys[1].'('.tag_keys[2].')'
endfunction

function! TagMgr_InitAlphaHashBucket(alphahash, val, idx)
    if a:val[0] =~ '\a'
        let bucket = a:val[0]
    else
        let bucket = s:miscbucket
    endif

    if a:idx < a:alphahash[bucket].min
        let a:alphahash[bucket].min = a:idx
    endif
    
    if a:idx > a:alphahash[bucket].max
        let a:alphahash[bucket].max = a:idx
    endif
endfunction


function! TagMgr_InitHashEntryFromVal(taghash, val, idx)
    let keyw = TagMgr_ExtractKeyFromTag(a:val) 
    let a:taghash[keyw] = {}
    let a:taghash[keyw].idx = a:idx
    let a:taghash[keyw].state = 0
endfunction


function! TagMgr_BuildHashFromTags(taglst)
    let taghash = {}
    let idx = 0
    for val in a:taglst
        call TagMgr_InitHashEntryFromVal(taghash, val, idx)
        let idx += 1
    endfor
    return taghash
endfunction

function! TagMgr_Error(msg)
    echohl WarningMsg
    echo s:pluginname.": " a:msg
    echohl None
endfunction

function! TagMgr_Info(msg)
    echohl Title 
    echo s:pluginname.": " a:msg
    echohl None
endfunction

function! TagMgr_DInfo(msg)
    echohl Title 
    echo  a:msg
    echohl None
endfunction

function! TagMgr_HandleFileRead(file, sym)
    let cmdoutput = TagMgr_GenerateTags(a:file.dir, a:file.name, "-", 0, 0)
    if cmdoutput != ''
        let a:sym.lst = split(cmdoutput, "\n")
        if len(a:sym.lst) < 3
            " 2 lines are usually an error
            call TagMgr_Error("Tag generation failed ". a:file.name)
            return
        endif
        let a:sym.hash = TagMgr_BuildHashFromTags(a:sym.lst)
    else
        call TagMgr_Error("System command failed")
    endif
endfunction

" Write stuff
function! TagMgr_MergeChangedTags (cursym, newlst)
    let lenlst = len(a:cursym.lst)

    for val in a:newlst
        let keyw = TagMgr_ExtractKeyFromTag(val)
        if !has_key(a:cursym.hash, keyw)
            call add(a:cursym.lst, val)
            let a:cursym.hash[keyw] = {}
            let a:cursym.hash[keyw].idx = lenlst
            let a:cursym.hash[keyw].state = 2
            let lenlst += 1
        else
            " mark as dirty
            if val != a:cursym.lst[a:cursym.hash[keyw].idx]
                let a:cursym.lst[a:cursym.hash[keyw].idx] = val
                let a:cursym.hash[keyw].state = 1
            else
            " keeper
                let a:cursym.hash[keyw].state = 3
            endif
        endif
    endfor
endfunction


function! TagMgr_BuildModList(srcsym)
    let newmods = 0
    let alphahash = TagMgr_BuildAlphaHash(a:srcsym.lst)
        
    for val in sort(copy(a:srcsym.lst), 'MyCompare')
        let keyw = TagMgr_ExtractKeyFromTag(val)
        if keyw[0] =~ '\a'
            let bucket = keyw[0]
        else
            let bucket = s:miscbucket
        endif
        
       let state = a:srcsym.hash[keyw].state
       if state !=3
            call add(alphahash[bucket].mods, keyw)
            let newmods += 1
       endif
    endfor

    if newmods > 0
        return alphahash
    else
        return []
    endif
endfunction


" write stuff

function! TagMgr_UpdateTags(tagfilename, newtaglst, newtaghash, actionhash)
    let tagfilelst = readfile(a:tagfilename)
    let [tagfilehash, tagfilealphahash] = TagMgr_BuildHashAndAlphaFromTags(tagfilelst)
    call TagMgr_CreateHashContinuity(tagfilealphahash)

    let stats = TagMgr_MergeMods(a:actionhash, a:newtaghash, a:newtaglst, tagfilehash, tagfilelst, tagfilealphahash)
    
    call TagMgr_Info('Updating... Additions -- '. stats.add . ' Deletions -- '. stats.del . ' Modified -- '. stats.mod)
    call writefile(tagfilelst, a:tagfilename)
endfunction

function! TagMgr_HandleFileWrite()
    let newtaglst = split(TagMgr_GenerateTags(b:tagMgrFileInfo.srcfile.dir, b:tagMgrFileInfo.srcfile.name, "-",0, 0), "\n")
    call TagMgr_MergeChangedTags(b:tagMgrFileInfo.sym, newtaglst)
    let alphaActionHash = TagMgr_BuildModList(b:tagMgrFileInfo.sym)

    let tagDbfile = b:tagMgrFileInfo.srcfile.dir .b:tagMgrFileInfo.DbFile

    if !empty(alphaActionHash)
        call TagMgr_UpdateTags(tagDbfile,
 \                             b:tagMgrFileInfo.sym.lst, 
 \                             b:tagMgrFileInfo.sym.hash, alphaActionHash)
        let b:tagMgrFileInfo.sym.lst = newtaglst
        let b:tagMgrFileInfo.sym.hash = TagMgr_BuildHashFromTags(newtaglst)
    else
        call TagMgr_EmulateTouch(tagDbfile)
    endif

endfunction

function! TagMgr_CreateHashContinuity (ahash)
    let lastmax = 0
    for akey in sort(keys(a:ahash))
        if a:ahash[akey].max == -1
            let a:ahash[akey].min = lastmax
            let a:ahash[akey].max = lastmax
        else
            let lastmax = a:ahash[akey].max
        endif
    endfor
endfunction


func! MyCompare(i1, i2)
    return a:i1 == a:i2 ? 0 : a:i1 > a:i2 ? -1 : 1
endfunc

function! HashCompare(i1, i2)
    return s:Ghash[a:i1].state == s:Ghash[a:i2].state ? 0 : s:Ghash[a:i1].state > s:Ghash[a:i2].state ? 1 : -1
endfunction


function! TagMgr_StatsInit()
    let stat = {}

    let stat.add = 0
    let stat.del = 0
    let stat.mod = 0

    return stat
endfunction


function! TagMgr_MergeMods(srcahash, srchash, srclst, dsthash, dstlst, alphahsh)
    let stats = TagMgr_StatsInit()
    for akey in reverse(keys(a:alphahsh))
        let t_stat = TagMgr_MergeOneAlphaBucket(a:srcahash[akey], a:alphahsh[akey], a:srclst, a:srchash, a:dsthash, a:dstlst)

        " update stats
        let stats.add += t_stat[0]
        let stats.mod += t_stat[1]
        let stats.del += t_stat[2]
    endfor

    return stats
endfunction

function! TagMgr_MergeOneAlphaBucket(bucket, dstbucket, srclst, srchash, dsthash, dstlst)
    let numadds=0
    let numdels=0
    let nummods=0
    
    let s:Ghash = a:srchash
    call sort(a:bucket.mods, 'HashCompare')
    unlet s:Ghash

    for val in a:bucket.mods
       let state = a:srchash[val].state
        " mark src as clean
       let a:srchash[val].state = 0
       if state == 2
        "add to dst
           call insert(a:dstlst, a:srclst[a:srchash[val].idx], a:dstbucket.min)
           call TagMgr_LogAddMsg("Adding tag ". a:dstlst[a:dstbucket.min])
           let numadds += 1
       elseif state == 1
        " replace
           let a:dstlst[a:dsthash[val].idx] = a:srclst[a:srchash[val].idx]
           call TagMgr_LogAddMsg("Modifying tag ". a:dstlst[a:dsthash[val].idx])
           let nummods += 1
       elseif state == 0
        " delete untouched
         call TagMgr_LogAddMsg("Deleting tag ". a:dstlst[a:dsthash[val].idx])
         unlet a:dstlst[a:dsthash[val].idx]
         unlet a:srchash[val]
         let numdels += 1
       endif
    endfor

    let a:dstbucket.max += (numadds-numdels)

    if numadds > 0
         let a:dstlst[a:dstbucket.min : a:dstbucket.max] = sort(a:dstlst[a:dstbucket.min : a:dstbucket.max])
    endif

    if numdels > 0
        " clean up the empty slots
        call TagMgr_RemoveNullListItems(a:dstlst, a:dstbucket.min)
    endif

    return [numadds, nummods, numdels]
endfunction

function! TagMgr_RemoveNullListItems (lst, top)
    while (a:lst[a:top] == '')
        remove a:lst[a:top]
    endwhile
endfunction

function! TagMgr_GetDbTStamp(fname)
    let relftime = getftime(a:fname)
    let pseudotime = getftime(a:fname."_tstamp")

    return max([relftime, pseudotime])
endfunction


function! TagMgr_CheckDbConsistency(tagDbCfg)
    let dir = a:tagDbCfg.dir
    let tagDbtstamp = TagMgr_GetDbTStamp(dir. a:tagDbCfg.name)

    for afile in a:tagDbCfg.srcfiles
       if getftime(dir.afile) > tagDbtstamp
         return -1
        endif
    endfor
    return 1
endfunction

function! TagMgr_EmulateTouch(fname)
    exec "redir! > ".a:fname. "_tstamp"
    silent! echo s:pluginname. " -- Tracking time stamp for ".a:fname
    exec "redir END"
endfunction

function! TagMgr_BuildTags(tagcfg, prompt)
    let ret = -1
    echohl Question
    if input(a:prompt. "\n Build database [".a:tagcfg.name . "] in directory [".a:tagcfg.dir. "] (y/n)? ", "y") == "y"
        call TagMgr_Info("Please wait while building database...")
        if match(a:tagcfg.filelst, "\*") != -1
            call TagMgr_GenerateTags(a:tagcfg.dir, g:TagMgr_SourceListFile, a:tagcfg.name, 1, 1)
        else
            call TagMgr_GenerateTags(a:tagcfg.dir, a:tagcfg.filelst, a:tagcfg.name, 1, 1)
        endif
        let ret = 1
        call TagMgr_Info("Done!")
    endif
    echohl None
    return ret
endfunction


function! TagMgr_BuildTagDbFileList ()
    let tagstr = ''
    for acfg in s:TagMgr_FileList
        let tagstr .= acfg.dir . acfg.name . ','
    endfor

    return tagstr
endfunction

function! TagMgr_SetTagDbFileList ()
    let &tags = TagMgr_BuildTagDbFileList()
endfunction


function! TagMgr_AutoTrackUpdate ()
    if g:TagMgr_AutoTrackDbs == 1
        call TagMgr_SetTagDbFileList()
    endif
endfunction


function! TagMgr_BuildSourceFileList(dirname, lstfilename, expr)
    if a:expr == 1
        let fileexp = fnamemodify(a:dirname, ":p") . a:lstfilename
        "let fileexp = fnameescape(fnamemodify(a:dirname, ":p") . a:lstfilename)
        let filelst = split(expand(fileexp), "\n")
        "return map(filelst, 'fnamemodify(v:val, ":t")')
        "
        let dir = escape(a:dirname, ' :\')
        call map(filelst, 'substitute(v:val, dir, "\.\\", "g")')
        call writefile(filelst, a:dirname. g:TagMgr_SourceListFile)
        return filelst
    else
        if filereadable(a:lstfilename) != 1
           call TagMgr_Error('Not able to read '. a:lstfilename) 
            return []
        endif
    endif
    return readfile(a:lstfilename)
endfunction

function! TagMgr_ShowDbs()
    let idx = 0
    let strfmt = "%-10s%-25s%-30s%-35s%-40s"
    let outtxt = printf(strfmt, "Index", "Database Name", "Directory", "Filelist/pattern", "Details")
    call TagMgr_DInfo(outtxt)
    for acfg in s:TagMgr_FileList
        let fname = acfg.dir . acfg.name
        let details = (getfsize(fname)/1024). 'Kb, ' . strftime("%Y %b %d %X", getftime(fname))	
        let outtxt = printf(strfmt, idx, acfg.name, acfg.dir, acfg.filelst, details)
        echo outtxt
        let idx += 1
    endfor
endfunction 


function! TagMgr_ShowDbDetail(idx)
    if len(s:TagMgr_FileList) <=  a:idx
        TagMgr_Error("TagDb not found for index ". a:idx)
        return
    endif
    
    let acfg = s:TagMgr_FileList[a:idx]

    call TagMgr_DInfo("Database Name: ".acfg.name."\t\t\t Directory: ". acfg.dir. "\t\t\t\File List/Pattern: ". acfg.filelst)
    call TagMgr_DInfo("Tracking files: ")
    echo acfg.srcfiles
endfunction


function! TagMgr_RemoveDb(idx)
    if len(s:TagMgr_FileList) <=  a:idx
        TagMgr_Error("TagDb not found for index ". a:idx)
        return
    endif

    call remove(s:TagMgr_FileList, a:idx)
    call TagMgr_AutoTrackUpdate()
endfunction


function! TagMgr_AddTagDbSet(Dbname, lstfilename, ...)
    if a:0 == 1
        let directory_set = split(a:1, ",")
    else
        let directory_set = ["."]
    endif

    for dir in directory_set
        call TagMgr_AddTagDb(a:Dbname, a:lstfilename, dir)
    endfor
endfunction

" Clean up and consolidate with other code
function! TagMgr_CheckAllDbConsistency()
    for acfg in s:TagMgr_FileList
       call TagMgr_CheckTagDbSanity(acfg)
    endfor
endfunction 


function! TagMgr_CheckTagDbSanity(tagDbCfg)
    let tagDbfound = 1
    let err = ''

    if filereadable(a:tagDbCfg.dir . a:tagDbCfg.name) != 1
        let err = 'Cannot find Tag database file ['. a:tagDbCfg.name . '] in directory ['. a:tagDbCfg.dir . '] for pattern/file '. a:tagDbCfg.filelst.'!'
        let tagDbfound = 0
    elseif TagMgr_CheckDbConsistency(a:tagDbCfg) == -1
        let tagDbfound = 0
        let err = "Tag Database out-of-date!"
    endif

    if tagDbfound == 0
       if TagMgr_BuildTags(a:tagDbCfg, err) == -1
          call TagMgr_Error("Tag database build failed")
          return -1
       endif
    endif

    return 1
endfunction

function! TagMgr_AddTagDb(Dbname, lstfilename, dirname)
    let tagDbCfg = {}

    let tagDbCfg.dir = fnamemodify(a:dirname, ":p")
    let tagDbCfg.name = a:Dbname
    
    let expr = 0
    if match(a:lstfilename, "\*") != -1
        let expr = 1
    endif
    let tagDbCfg.filelst = a:lstfilename
    let tagDbCfg.srcfiles = TagMgr_BuildSourceFileList(tagDbCfg.dir, tagDbCfg.filelst, expr)

    if empty(tagDbCfg.srcfiles)
        call TagMgr_Error("Could not build source list for file/pattern ". a:lstfilename. " in directory ". a:dirname)
        return -1
    endif
 
    " Don't do this when Vim is starting up
    if s:TagMgr_Init == 1    
        if TagMgr_CheckTagDbSanity(tagDbCfg) != 1
            return
        endif   

        call TagMgr_Info("Added tags database!")
        call TagMgr_AutoTrackUpdate()
    endif
    call add(s:TagMgr_FileList, tagDbCfg)
endfunction

function! TagMgr_BuildDbRefs()
    if g:TagMgrPathAbsolute == 0
        call map(s:TagMgr_FileList, g:TagMgr_RootPath. v:val)
    endif
endfunction

function! TagMgr_ReInitTagDbs()
    for acfg in s:TagMgr_FileList
        " only reinit patterns
        if match(acfg.filelst , "\*") != -1
            let acfg.srcfiles = TagMgr_BuildSourceFileList(acfg.dir, acfg.filelst, 1)
        endif
    endfor
endfunction


" Clean up required
function! TagMgr_FindTagDbForFile (filename)

    let filetail = fnamemodify(a:filename, ":t")

    " This is weird, this check is to hide some bug
    if filetail == ''
        return [[], -1]
    endif

    for acfg in s:TagMgr_FileList
       
       let idx = match(acfg.srcfiles, filetail.'$')

       while idx != -1
        " see if exact match
        let filestr = simplify(acfg.dir . acfg.srcfiles[idx])
        if stridx(filestr, a:filename) > -1
            return [acfg, idx]
        endif
       
       call TagMgr_LogAddMsg("Skipping match ". idx. " for ". a:filename)
       let idx = match(acfg.srcfiles, filetail.'$', idx+1)
       endwhile

    endfor

    return [[], -1]
endfunction


function! TagMgr_HandleFileUnloadEvent()
    if exists("b:tagDbMgrInfo")
        call TagMgr_LogAddMsg("Cleaning up state for ". expand('%'))
        unlet b:tagDbMgrInfo
    endif
endfunction


function! TagMgr_FileReadInitState()
    let [tagDbCfg, idx] = TagMgr_FindTagDbForFile(expand('%'))
    call TagMgr_LogAddMsg("FileReadInit triggered for ". expand('%'))
    if idx != -1
        let b:tagMgrFileInfo = {}
        let b:tagMgrFileInfo.srcfile = {}
        let b:tagMgrFileInfo.sym = {}
        let b:tagMgrFileInfo.sym.hash = {}
        let b:tagMgrFileInfo.sym.lst = []

        let b:tagMgrFileInfo.DbFile = tagDbCfg.name
        let b:tagMgrFileInfo.srcfile.name = tagDbCfg.srcfiles[idx]
        let b:tagMgrFileInfo.srcfile.dir = tagDbCfg.dir

    else
        if exists("b:tagMgrFileInfo")
            unlet b:tagMgrFileInfo
        endif
    endif
endfunction
    
function! TagMgr_FileReadEvent()
    call TagMgr_FileReadInitState()
    if exists("b:tagMgrFileInfo")
        call TagMgr_LogAddMsg("FileReadEvent triggered for ". expand('%'))
        call TagMgr_HandleFileRead(b:tagMgrFileInfo.srcfile, b:tagMgrFileInfo.sym)
    endif
endfunction

function! TagMgr_HandleNewFileCreation()
        call TagMgr_LogAddMsg("Rebuilding source list for ". expand('%'))
        call TagMgr_ReInitTagDbs()
        unlet b:tagMgrFileNew
        call TagMgr_FileReadInitState()
endfunction

function! TagMgr_FileWriteEvent()
    if exists("b:tagMgrFileNew")
        call TagMgr_HandleNewFileCreation()
    endif
    if exists("b:tagMgrFileInfo") 
        call TagMgr_HandleFileWrite()
    endif
endfunction

function! TagMgr_HandleFileNewEvent(newfile)
    if a:newfile == 1 || (a:newfile == 0  && expand('<afile>') == '')
        call setbufvar(str2nr(expand('<abuf>')), "tagMgrFileNew", 1)
        call TagMgr_LogAddMsg("Marking new file for ". expand('%'))
    endif
endfunction

augroup TagMgr_Events
    au!
    autocmd BufWritePost * call TagMgr_FileWriteEvent()
    autocmd BufReadPre * call TagMgr_FileReadEvent()
    autocmd BufUnload * call TagMgr_HandleFileUnloadEvent()
    autocmd BufCreate * call TagMgr_HandleFileNewEvent(0)
    autocmd BufNewFile * call TagMgr_HandleFileNewEvent(1)
    autocmd FocusGained * call TagMgr_CheckAllDbConsistency()
    autocmd VimEnter * let s:TagMgr_Init=1 | call TagMgr_CheckAllDbConsistency()
augroup END

command! -nargs=+ TagManagerAddDb call TagMgr_AddTagDbSet(<f-args>)
command! -nargs=0 TagManagerShowDbs call TagMgr_ShowDbs()
command! -nargs=1 TagManagerRemoveDb call TagMgr_RemoveDb(<f-args>)
command! -nargs=1 TagManagerShowDbDetail call TagMgr_ShowDbDetail(<f-args>)
command! -nargs=0 TagManagerShowLog call TagMgr_LogShowMsgs()

" restore 'cpo'
let &cpoptions = s:cpo_save
unlet s:cpo_save


