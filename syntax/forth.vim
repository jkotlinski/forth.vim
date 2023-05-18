" Vim syntax file
" Language:		Forth
" Maintainer:		Johan Kotlinski <kotlinski@gmail.com>
" Previous Maintainer:	Christian V. J. Brüssow <cvjb@cvjb.de>
" Last Change:		2023 Apr 12
" Filenames:		*.f,*.fs,*.ft,*.fth,*.4th
" URL:			https://github.com/jkotlinski/forth.vim

" quit when a syntax file was already loaded
if exists("b:current_syntax")
    finish
endif

let s:cpo_save = &cpo
set cpo&vim

" Synchronization method
syn sync ccomment
syn sync maxlines=200

" I use gforth, so I set this to case ignore
syn case ignore

" Characters allowed in keywords
" I don't know if 128-255 are allowed in ANS-FORTH
syn iskeyword 33-126,128-255

" when wanted, highlight trailing white space
if exists("forth_space_errors")
    if !exists("forth_no_trail_space_error")
        syn match forthSpaceError display excludenl "\s\+$"
    endif
    if !exists("forth_no_tab_space_error")
        syn match forthSpaceError display " \+\t"me=e-1
    endif
endif

" Core words

" basic mathematical and logical operators
syn keyword forthOperators + - * / MOD /MOD NEGATE ABS MIN MAX
syn keyword forthOperators AND OR XOR LSHIFT RSHIFT INVERT 2* 2/ 1+
syn keyword forthOperators 1-
syn keyword forthOperators */ */MOD M* UM* UM/MOD FM/MOD SM/REM
syn keyword forthOperators 0< 0<> 0= 0> < <> = > U<
syn keyword forthOperators U> WITHIN

" non-standard basic mathematical and logical operators
syn keyword forthOperators NOT
syn keyword forthOperators 2+ 2- 8* UNDER+
syn keyword forthOperators 0<= 0>= <= >= U<=
syn keyword forthOperators U>= ?NEGATE
syn keyword forthOperators ?DNEGATE

" various words that take an input and do something with it
syn keyword forthFunction . U. .R U.R

" stack manipulations
syn keyword forthStack DROP NIP DUP OVER TUCK SWAP ROT ?DUP PICK ROLL
syn keyword forthStack 2DROP 2DUP 2OVER 2SWAP
syn keyword forthRStack >R R> R@ 2>R 2R> 2R@

" non-standard stack manipulations
syn keyword forthStack -ROT
syn keyword forthStack 3DUP 4DUP 5DUP 3DROP 4DROP 5DROP 8DROP 4SWAP 4ROT
syn keyword forthStack 4-ROT 4TUCK 8SWAP 8DUP
syn keyword forthRStack RDROP
syn keyword forthRstack 4>R 4R> 4R@ 4RDROP

" stack pointer manipulations
syn keyword forthSP DEPTH

" non-standard stack pointer manipulations
syn keyword forthSP SP@ SP! FP@ FP! RP@ RP! LP@ LP!

" address operations
syn keyword forthMemory @ ! +! C@ C! 2@ 2!
syn keyword forthAdrArith CHARS CHAR+ CELLS CELL+ ALIGN ALIGNED
syn keyword forthAdrArith ALLOT
syn keyword forthMemBlks MOVE ERASE FILL UNUSED

" non-standard address operations
syn keyword forthAdrArith CELL FLOAT
syn keyword forthAdrArith MAXALIGN MAXALIGNED CFALIGN CFALIGNED
syn keyword forthAdrArith ADDRESS-UNIT-BITS

" conditionals
syn keyword forthCond IF ELSE THEN CASE OF ENDOF ENDCASE

" non-standard conditionals
syn keyword forthCond ENDIF ?DUP-IF
syn keyword forthCond ?DUP-0=-IF

" iterations
syn keyword forthLoop BEGIN WHILE REPEAT UNTIL AGAIN
syn keyword forthLoop ?DO LOOP I J DO +LOOP
syn keyword forthLoop UNLOOP LEAVE EXIT RECURSE

" non-standard iterations
syn keyword forthLoop K +DO U+DO -DO U-DO -LOOP
syn keyword forthLoop ?LEAVE DONE FOR NEXT

" new words
syn match forthColonDef "\<:\s*[^ \t]\+\>"
syn keyword forthEndOfColonDef ;
syn keyword forthDefine CONSTANT VARIABLE
syn keyword forthDefine CREATE VALUE TO DEFER IS DOES> IMMEDIATE
syn keyword forthDefine POSTPONE EXECUTE
syn keyword forthDefine ]
syn keyword forthDefine LITERAL
syn keyword forthDefine STATE BUFFER: MARKER
syn keyword forthDefine , C, COMPILE, '
syn match forthDefine "\<\[\>"
syn match forthDefine "\<\[']\>"
syn match forthDefine "\<\[COMPILE]\>"

" non-standard new words
syn match forthClassDef "\<:CLASS\s*[^ \t]\+\>"
syn match forthObjectDef "\<:OBJECT\s*[^ \t]\+\>"
syn match forthColonDef "\<:M\s*[^ \t]\+\>"
syn keyword forthEndOfColonDef ;M
syn keyword forthEndOfClassDef ;CLASS
syn keyword forthEndOfObjectDef ;OBJECT
syn keyword forthDefine USER <BUILDS
syn keyword forthDefine COMPILE-ONLY COMPILE RESTRICT INTERPRET
syn keyword forthDefine CREATE-INTERPRET/COMPILE INTERPRETATION>
syn keyword forthDefine <INTERPRETATION COMPILATION> <COMPILATION LASTXT
syn keyword forthDefine COMP' POSTPONE, FIND-NAME NAME>INT NAME?INT NAME>COMP
syn keyword forthDefine C; CVARIABLE
syn keyword forthDefine 2, F,
syn match forthDefine "\<\[COMP']\>"

" non-standard debugging
syn keyword forthDebug PRINTDEBUGDATA PRINTDEBUGLINE
syn match forthDebug "\<\~\~\>"

" basic character operations
syn keyword forthCharOps WORD TYPE EMIT KEY
syn keyword forthCharOps CR BL COUNT SPACE SPACES
" recognize 'char (' or '[CHAR] (' correctly, so it doesn't
" highlight everything after the paren as a comment till a closing ')'
syn match forthCharOps '\<CHAR\s\S\s'
syn match forthCharOps '\<\[CHAR]\s\S\s'

" non-standard basic character operations
syn keyword forthCharOps (.) EXPECT
syn keyword forthCharOps TIB

" char-number conversion
syn keyword forthConversion <# # #> #S
syn keyword forthConversion HOLD HOLDS S>D SIGN >NUMBER

" non-standard char-number conversion
syn keyword forthConversion <<# #>> (NUMBER) (NUMBER?) CONVERT
syn keyword forthConversion DIGIT DPL HLD NUMBER

" interpreter, wordbook, compiler
syn keyword forthForth ABORT >BODY HERE
syn keyword forthForth PAD
syn keyword forthForth >IN ACCEPT ENVIRONMENT? EVALUATE QUIT SOURCE ACTION-OF
syn keyword forthForth DEFER! DEFER@ PARSE PARSE-NAME REFILL RESTORE-INPUT
syn keyword forthForth SAVE-INPUT SOURCE-ID

" non-standard interpreter, wordbook, compiler
syn keyword forthForth COLD >NEXT >LINK CFA >VIEW
syn keyword forthForth VIEW VIEW> N>LINK NAME> LINK> L>NAME
syn keyword forthForth BODY> ASSERT( ASSERT0( ASSERT1( ASSERT2( ASSERT3( )

" booleans
syn match forthBoolean "\<\%(TRUE\|FALSE\)\>"

" numbers
syn keyword forthMath DECIMAL HEX BASE
syn match forthInteger '\<-\=\d\+\.\=\>'
syn match forthInteger '\<#-\=\d\+\.\=\>'
syn match forthInteger '\<\$-\=\x\+\.\=\>'
syn match forthInteger '\<%-\=[01]\+\.\=\>'
syn match forthFloat '\<[+-]\=\d\+\.\=\d*[DdEe][+-]\=\d*\>'

" Strings

" Words that end with " are assumed to start string parsing.
" This includes standard words: S" C" ."
syn region forthString matchgroup=forthString start=+\<\S\+"\s+ end=+"\>+ end=+$+ contains=@Spell
" Matches s\"
syn region forthString matchgroup=forthString start=+\<S\\"\s+ end=+"\>+ end=+$+ contains=@Spell,forthEscape

syn match forthEscape +\C\\[abeflmnqrtvz"\\]+ contained
syn match forthEscape "\C\\x\x\x" contained

" Comments

" Some special, non-FORTH keywords
syn match forthTodo contained "\<\%(TODO\|FIXME\|XXX\)\%(\>\|:\)\@="

" XXX If you find this overkill you can remove it. This has to come after the
" highlighting for numbers and booleans otherwise it has no effect.
syn region forthComment start='\<\%(0\|FALSE\)\s\+\[IF]' end='\<\[ENDIF]' end='\<\[THEN]' contains=forthTodo

syn match forthComment '\<\\\>.*$' contains=@Spell,forthTodo,forthSpaceError
syn match forthComment '\<\.(\s[^)]*)\>' contains=@Spell,forthTodo,forthSpaceError
syn region forthComment start='\<(\>' end=')\>' contains=@Spell,forthTodo,forthSpaceError

" Abort"
syn region forthForth start=+\<ABORT"\s+ end=+"\>+ end=+$+

" Include files
syn match forthInclude '\<INCLUDE\s\+\k\+'
syn match forthInclude '\<REQUIRE\s\+\k\+'

" Non-standard Include files
syn match forthInclude '^FLOAD\s\+'
syn match forthInclude '^NEEDS\s\+'

" The optional Block word set
syn keyword forthBlocks BLK BLOCK BUFFER FLUSH LOAD SAVE-BUFFERS UPDATE
syn keyword forthBlocks EMPTY-BUFFERS LIST SCR THRU

" Non-standard Block words
syn keyword forthBlocks OPEN-BLOCKS USE --> BLOCK-OFFSET
syn keyword forthBlocks GET-BLOCK-FID BLOCK-POSITION EMPTY-BUFFER UPDATED?
syn keyword forthBlocks SAVE-BUFFER +LOAD +THRU BLOCK-INCLUDED

" The optional Double-Number word set
syn keyword forthConversion D>S
syn keyword forthDefine 2CONSTANT 2LITERAL 2VALUE 2VARIABLE
syn keyword forthFunction D. D.R
syn keyword forthOperators DABS D= DMAX DMIN D- DNEGATE D+ D2/ D2* DU< D0=
syn keyword forthOperators D0< D< M+ M*/
syn keyword forthStack 2ROT

" Non-standard Double-Number words
syn keyword forthOperators D0<= D0<> D0> D0>= D<= D<> D> D>= DU<= DU> DU>=
syn keyword forthStack 2NIP 2TUCK 2-ROT 2RDROP

" The optional Exception word set
" Handled as Core words - ABORT ABORT"
syn keyword forthCond CATCH THROW

" The optional File-Access word set
" Handled as Core words - INCLUDE REFILL REQUIRE SOURCE-IDS S\" S" (
syn keyword forthFileMode BIN R/O R/W W/O
syn keyword forthFileWords CLOSE-FILE CREATE-FILE DELETE-FILE FILE-POSITION
syn keyword forthFileWords FILE-SIZE FILE-STATUS FLUSH-FILE INCLUDE-FILE
syn keyword forthFileWords INCLUDED OPEN-FILE READ-FILE READ-LINE RENAME-FILE
syn keyword forthFileWords REPOSITION-FILE REQUIRED RESIZE-FILE WRITE-FILE
syn keyword forthFileWords WRITE-LINE

" Non-standard File-Access words
syn keyword forthFileWords KEY-FILE KEY?-FILE EMIT-FILE SLURP-FILE SLURP-FID
syn keyword forthFileWords STDIN STDOUT STDERR

" The optional Facility word set
syn keyword forthDefine BEGIN-STRUCTURE END-STRUCTURE FIELD: CFIELD: +FIELD
syn keyword forthCharOps AT-XY KEY? EKEY EKEY? EKEY>CHAR EKEY>FKEY EMIT? PAGE
syn keyword forthCharOps K-ALT-MASK K-CTRL-MASK K-DELETE K-DOWN K-END K-F3
syn keyword forthCharOps K-F4 K-F5 K-F6 K-F7 K-F8 K-F9 K-F1 K-F11 K-F12 K-F10
syn keyword forthCharOps K-F2 K-HOME K-INSERT K-LEFT K-NEXT K-PRIOR K-RIGHT
syn keyword forthCharOps K-SHIFT-MASK K-UP
syn keyword forthForth MS TIME&DATE

" The optional Floating-Point word set
syn keyword forthOperators F+ F- F* F/ FNEGATE FABS FMAX FMIN FLOOR FROUND
syn keyword forthOperators F** FSQRT FEXP FEXPM1 FLN FLNP1 FLOG FALOG FSIN
syn keyword forthOperators FCOS FSINCOS FTAN FASIN FACOS FATAN FATAN2 FSINH
syn keyword forthOperators FCOSH FTANH FASINH FACOSH FATANH F~ FTRUNC
syn keyword forthOperators F0= F0< F<
syn keyword forthFunction F. FE. FS.
syn keyword forthFunction PRECISION SET-PRECISION REPRESENT
syn keyword forthFStack FDROP FDUP FOVER FSWAP FROT
syn keyword forthSP FDEPTH
syn keyword forthMemory F@ F! SF@ SF! DF@ DF!
syn keyword forthAdrArith FLOATS FLOAT+ FALIGN FALIGNED SFLOATS SFLOAT+
syn keyword forthAdrArith SFALIGN SFALIGNED DFLOATS DFLOAT+ DFALIGN DFALIGNED
syn keyword forthDefine FCONSTANT FLITERAL FVALUE FVARIABLE
syn keyword forthDefine FFIELD: DFFIELD: SFFIELD:
syn keyword forthConversion S>F D>F F>S F>D >FLOAT

" Non-standard Floating-Point words
syn keyword forthOperators F~REL F~ABS F2* F2/ 1/F
syn keyword forthFStack FNIP FTUCK

" The optional Locals word set
syn keyword forthForth (LOCAL)
syn region forthLocals start="\<{:\>" end="\<:}\>"
syn region forthLocals start="\<LOCALS|\>" end="\<|\>"

" Non-standard Locals words
syn region forthLocals start="\<{\>" end="\<}\>"

" The optional Memory-Allocation word set
syn keyword forthMemory ALLOCATE FREE RESIZE

" The optional Programming-Tools wordset
syn match forthDefine "\<\[DEFINED]\>"
syn match forthDefine "\<\[ELSE]\>"
syn match forthDefine "\<\[IF]\>"
syn match forthDefine "\<\[THEN]\>"
syn match forthDefine "\<\[UNDEFINED]\>"
syn keyword forthAssembler ASSEMBLER CODE END-CODE ;CODE
syn keyword forthCond AHEAD CS-PICK CS-ROLL
syn keyword forthDebug .S ? DUMP SEE WORDS
syn keyword forthDefine NAME>COMPILE NAME>INTERPRET NAME>STRING SYNONYM
syn keyword forthDefine TRAVERSE-WORDLIST
syn keyword forthForth BYE FORGET
syn keyword forthStack NR> N>R
syn keyword forthVocs EDITOR

" Non-standard Programming-Tools words
syn keyword forthAssembler FLUSH-ICACHE
syn match forthDefine "\<\[IFDEF]\>"
syn match forthDefine "\<\[IFUNDEF]\>"
syn match forthDefine "\<\[ENDIF]\>"
syn match forthDefine "\<\[?DO]\>"
syn match forthDefine "\<\[DO]\>"
syn match forthDefine "\<\[LOOP]\>"
syn match forthDefine "\<\[+LOOP]\>"
syn match forthDefine "\<\[NEXT]\>"
syn match forthDefine "\<\[BEGIN]\>"
syn match forthDefine "\<\[UNTIL]\>"
syn match forthDefine "\<\[AGAIN]\>"
syn match forthDefine "\<\[WHILE]\>"
syn match forthDefine "\<\[REPEAT]\>"

" The optional Search-Order word set
" Handled as Core words - FIND
syn keyword forthVocs ALSO DEFINITIONS FIND FORTH-WORDLIST FORTH GET-CURRENT
syn keyword forthVocs GET-ORDER ONLY ORDER PREVIOUS SEARCH-WORDLIST
syn keyword forthVocs SET-CURRENT SET-ORDER WORDLIST

" Non-standard Search-Order words
syn keyword forthVocs ROOT SEAL VOCS CONTEXT #VOCS VOCABULARY

" The optional String word set
syn keyword forthFunction -TRAILING /STRING BLANK CMOVE CMOVE> COMPARE
syn keyword forthFunction SEARCH SLITERAL REPLACES SUBSTITUTE UNESCAPE

" The optional Extended-Character word set
" Handled as Core words - [CHAR] CHAR and PARSE
syn keyword forthCharOps EKEY>XCHAR
syn keyword forthString -TRAILING-GARBAGE
syn keyword forthDefine XC,
syn keyword forthMemory XC@+ XC!+ XC!+?
syn keyword forthAdrArith XCHAR- XCHAR+ +X/STRING X\\STRING-
syn keyword forthCharOps X-SIZE X-WIDTH XC-SIZE XC-WIDTH XEMIT XKEY XKEY?
syn keyword forthConversion XHOLD

" Define the default highlighting.
hi def link forthBoolean Boolean
hi def link forthTodo Todo
hi def link forthOperators Operator
hi def link forthMath Number
hi def link forthInteger Number
hi def link forthFloat Float
hi def link forthStack Special
hi def link forthRstack Special
hi def link forthFStack Special
hi def link forthSP Special
hi def link forthMemory Function
hi def link forthAdrArith Function
hi def link forthMemBlks Function
hi def link forthCond Conditional
hi def link forthLoop Repeat
hi def link forthColonDef Define
hi def link forthEndOfColonDef Define
hi def link forthDefine Define
hi def link forthDebug Debug
hi def link forthAssembler Include
hi def link forthCharOps Character
hi def link forthConversion String
hi def link forthForth Statement
hi def link forthVocs Statement
hi def link forthEscape Special
hi def link forthString String
hi def link forthComment Comment
hi def link forthClassDef Define
hi def link forthEndOfClassDef Define
hi def link forthObjectDef Define
hi def link forthEndOfObjectDef Define
hi def link forthInclude Include
hi def link forthLocals Type " nothing else uses type and locals must stand out
hi def link forthFileMode Function
hi def link forthFunction Function
hi def link forthFileWords Statement
hi def link forthBlocks Statement
hi def link forthSpaceError Error

let b:current_syntax = "forth"

let &cpo = s:cpo_save
unlet s:cpo_save
" vim:ts=8:sw=4:nocindent:smartindent:

