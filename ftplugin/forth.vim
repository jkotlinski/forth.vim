" Vim filetype plugin
" Language:	Forth
" Maintainer:	Johan Kotlinski <kotlinski@gmail.com>
" Last Change:	2023 Apr 12
" URL:		https://github.com/jkotlinski/forth.vim

if exists("b:did_ftplugin")
  finish
endif
let b:did_ftplugin = 1

let s:cpo_save = &cpo
set cpo&vim

setlocal commentstring=\\\ %s
setlocal comments=b:\\,s:.(,mb:\ ,e:)
setlocal formatoptions-=t formatoptions+=croql
setlocal iskeyword=!,@,33-35,%,$,38-64,A-Z,91-96,a-z,123-126,128-255

let b:undo_ftplugin = "setl cms< com< fo< isk<"

if (has("gui_win32") || has("gui_gtk")) && !exists("b:browsefilter")
  let b:browsefilter = "Forth Source Files (*.f *.fs *.ft *.fth *.4th)\t*.f;*.fs;*.ft;*.fth;*.4th\n" ..
	\	       "All Files (*.*)\t*.*\n"
  let b:undo_ftplugin ..= " | unlet! b:browsefilter"
endif

let &cpo = s:cpo_save
unlet s:cpo_save
