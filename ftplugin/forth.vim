" Vim filetype plugin
" Language:	Forth
" Maintainer:	Johan Kotlinski <kotlinski@gmail.com>
" Last Change:	2023 May 02
" URL:		https://github.com/jkotlinski/forth.vim

if exists("b:did_ftplugin")
  finish
endif
let b:did_ftplugin = 1

let s:cpo_save = &cpo
set cpo&vim

setlocal commentstring=\\\ %s
setlocal comments=s:(,mb:\ ,e:),b:\\
setlocal iskeyword=33-126,128-255

let b:undo_ftplugin = "setl cms< com< isk<"

if exists("loaded_matchit") && !exists("b:match_words")
  let b:match_words = '\<\:\%(noname\)\=\>:\<exit\>:\<;\>,' ..
	\	      '\<if\>:\<else\>:\<then\>,' ..
	\	      '\[if]:\[else]:\[then],' ..
	\	      '\<?\=do\>:\<leave\>:\<+\=loop\>,' ..
	\	      '\<case\>:\<endcase\>,' ..
	\	      '\<of\>:\<endof\>,' ..
	\	      '\<begin\>:\<while\>:\<\%(again\|repeat\|until\)\>,' ..
	\	      '\<code\>:\<end-code\>,' ..
	\	      '\<begin-structure\>:\<end-structure\>'
  let b:undo_ftplugin ..= "| unlet! b:match_ignorecase b:match_words"
endif

if (has("gui_win32") || has("gui_gtk")) && !exists("b:browsefilter")
  let b:browsefilter = "Forth Source Files (*.f *.fs *.ft *.fth *.4th)\t*.f;*.fs;*.ft;*.fth;*.4th\n" ..
	\	       "All Files (*.*)\t*.*\n"
  let b:undo_ftplugin ..= " | unlet! b:browsefilter"
endif

if !exists("no_plugin_maps") && !exists("no_forth_maps")
  let colon = '\<:\%(noname\)\=\>'
  let semicolon = '\<;\>'
  let skip = 'synIDattr(synID(line("."), col("."), 1), "name") =~? "comment\\|string"'

  for mode in ["n", "o", "x"]
    exe $'{mode}noremap <silent><buffer> ]] <ScriptCmd>call search(colon, "Ws", 0, 0, skip)<CR>'
    exe $'{mode}noremap <silent><buffer> [[ <ScriptCmd>call search(colon, "bWs", 0, 0, skip)<CR>'
    exe $'{mode}noremap <silent><buffer> ][ <ScriptCmd>call search(semicolon, "Ws", 0, 0, skip)<CR>'
    exe $'{mode}noremap <silent><buffer> [] <ScriptCmd>call search(semicolon, "bWs", 0, 0, skip)<CR>'
    for map_ in ["]]", "[[", "][", "[]"]
      let b:undo_ftplugin ..= $" | silent! execute '{mode}unmap <buffer> {map_}'"
    endfor
  endfor
endif

let &cpo = s:cpo_save
unlet s:cpo_save
