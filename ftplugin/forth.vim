" Vim filetype plugin
" Language:	Forth
" Maintainer:	Johan Kotlinski <kotlinski@gmail.com>
" Last Change:	2023 May 05
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

let include_patterns =<< trim EOL

  \<\%(INCLUDE\|REQUIRE\)\>\s\+\zs\k\+\ze
  \<S"\s\+\zs[^"]*\ze"\s\+\%(INCLUDED\|REQUIRED\)\>
EOL
let &l:include = $'\c{ include_patterns[1:]->join('\|') }'

let define_patterns =<< trim EOL
  :
  [2F]\=CONSTANT
  [2F]\=VALUE
  [2F]\=VARIABLE
  BEGIN-STRUCTURE
  BUFFER:
  CODE
  CREATE
  MARKER
  SYNONYM
EOL
let &l:define = $'\c\<\%({ define_patterns->join('\|') }\)'

let b:undo_ftplugin = "setl cms< com< def< inc< isk<"

if exists("loaded_matchit") && !exists("b:match_words")
  let b:match_ignorecase = 1
  let b:match_words = '\<\:\%(NONAME\)\=\>:\<EXIT\>:\<;\>,' ..
	\	      '\<IF\>:\<ELSE\>:\<THEN\>,' ..
	\	      '\[IF]:\[ELSE]:\[THEN],' ..
	\	      '\<?\=DO\>:\<LEAVE\>:\<+\=LOOP\>,' ..
	\	      '\<CASE\>:\<ENDCASE\>,' ..
	\	      '\<OF\>:\<ENDOF\>,' ..
	\	      '\<BEGIN\>:\<WHILE\>:\<\%(AGAIN\|REPEAT\|UNTIL\)\>,' ..
	\	      '\<CODE\>:\<END-CODE\>,' ..
	\	      '\<BEGIN-STRUCTURE\>:\<END-STRUCTURE\>'
  let b:undo_ftplugin ..= "| unlet! b:match_ignorecase b:match_words"
endif

if (has("gui_win32") || has("gui_gtk")) && !exists("b:browsefilter")
  let b:browsefilter = "Forth Source Files (*.f *.fs *.ft *.fth *.4th)\t*.f;*.fs;*.ft;*.fth;*.4th\n" ..
	\	       "All Files (*.*)\t*.*\n"
  let b:undo_ftplugin ..= " | unlet! b:browsefilter"
endif

let &cpo = s:cpo_save
unlet s:cpo_save
