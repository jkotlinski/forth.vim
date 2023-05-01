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

let &cpo = s:cpo_save
unlet s:cpo_save
