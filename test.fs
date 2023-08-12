( This file tests Forth syntax highlighting and plugin functionality.
  The goal is that it will cover all functionality.
  Please update this file as you make changes. )

\ --- Comments

\ comments are highlighted like this...
( ...or this... )
( ...multi-line comments
  are fine too! )
.( Somehow, this also counts as a comment. )
0 [IF] comment [ENDIF]
0 [IF] comment [THEN]
FALSE [IF] comment [ENDIF]
FALSE [IF] comment [THEN]

\ comment folding

(    Test: Comment folding
  Execute: :set foldmethod=syntax
   Result: These three lines should be folded.)

\    Test: Multiple line comment folding
\ Execute: :set foldmethod=syntax
\  Result: These three lines should be folded.

\ Verify that comments terminate at the closing ) delimiter.
( paren)foo
.( dot-paren)foo

\ --- Strings

\ Verify that strings have their own color.
S" s-quote"
C" c-quote"
S\" s-backslash-quote"
." dot-quote"
FOO" custom string matching word"

\ Verify that escaped characters have their own color.
S\" \a\b\e\f\l\m\n\q\r\t\v\z\"\xff\x00\\"

\ Upper-case characters cannot be escaped.
S\" \A\B\E\F\L\M\N\Q\R\T\V\Z\Xff\X00"

\ Verify that strings terminate at the closing " delimiter.
S" s-quote"foo
C" c-quote"foo
S\" s-backslash-quote"foo
." dot-quote"foo
CUSTOM" custom string matching word"foo

\ --- Characters

'a'
' ' \ consecutive tick

\ --- Spell Checking

( activate spell checking by ":set spell"
then, verify that the below "mispelt"
words are highlighted. )
S" mispelt"
C" mispelt"
( mispelt )
\ mispelt
S\" mispelt"

\ --- To Do

\ TODO FIXME XXX
\ TODO: or not TODO that is...
\ FIXME: ...
\ XXX: ...

\ not matched outside comments
TODO FIXME XXX

\ --- Ignore Case

dup DUP Dup

\ --- iskeyword

dup-drop \ dup and drop should not be highlighted

\ --- Keywords

\ basic mathematical and logical operators
  \ Core
* */ */MOD + - / /MOD 0< 0= 1+ 1- 2* 2/ < = > ABS AND FM/MOD
INVERT LSHIFT M* MAX MIN MOD NEGATE OR RSHIFT SM/REM U< UM*
UM/MOD XOR
  \ Core extension
0<> 0> <> U> WITHIN
  \ Forth-79
U* U/ U/MOD
  \ Forth-79, Forth-83
NOT
  \ Forth-83
2+ 2-
  \ Non-standard
0<= 0>= 8* <= >= ?DNEGATE ?NEGATE U<= U>= UNDER+

\ various words that take an input and do something with it
  \ Core
. U.
  \ Core extension
.R U.R

\ stack manipulations
  \ Core
2DROP 2DUP 2OVER 2SWAP >R ?DUP DROP DUP OVER R> R@ ROT SWAP
  \ Core extension
NIP PICK ROLL TUCK
2>R 2R> 2R@
  \ Non-standard
-ROT 3DROP 3DUP 4-ROT 4DROP 4DUP 4ROT 4SWAP 4TUCK 5DROP 5DUP
8DROP 8DUP 8SWAP
4>R 4R> 4R@ 4RDROP RDROP

\ stack pointer manipulations
  \ Core
DEPTH
  \ Non-standard
SP@ SP! FP@ FP! RP@ RP! LP@ LP!

\ address operations
  \ Core
!  +!  2!  2@ @ C!  C@
ALIGN ALIGNED ALLOT CELL+ CELLS CHAR+ CHARS
FILL MOVE
  \ Core extension
ERASE UNUSED
  \ Non-standard
ADDRESS-UNIT-BITS CELL CFALIGN CFALIGNED FLOAT MAXALIGN MAXALIGNED

\ conditionals
  \ Core
ELSE IF THEN
  \ Core extension
CASE ENDCASE ENDOF OF
  \ Non-standard
?DUP-0=-IF ?DUP-IF ENDIF

\ iterations
  \ Core
+LOOP BEGIN DO EXIT I J LEAVE LOOP RECURSE REPEAT UNLOOP UNTIL
WHILE
  \ Core extension
?DO AGAIN
  \ Non-standard
+DO -DO -LOOP ?LEAVE DONE FOR K NEXT U+DO U-DO

\ new words
  \ Core
: ;
' , C, CONSTANT CREATE DOES> EXECUTE IMMEDIATE LITERAL POSTPONE
STATE VARIABLE ] ['] [
  \ Core extension
:NONAME BUFFER: COMPILE, DEFER IS MARKER TO VALUE [COMPILE]
  \ Forth-79, Forth-83
COMPILE
  \ Non-standard
:CLASS ;CLASS :OBJECT ;OBJECT :M ;M
2, <BUILDS <COMPILATION <INTERPRETATION C; COMP' COMPILATION>
COMPILE-ONLY CREATE-INTERPRET/COMPILE CVARIABLE F, FIND-NAME
INTERPRET INTERPRETATION> LASTXT NAME>COMP NAME>INT NAME?INT
POSTPONE, RESTRICT USER [COMP']

\ basic character operations
  \ Core
BL CHAR ( COUNT CR EMIT FIND KEY SPACE SPACES TYPE WORD [CHAR] ( 
  \ Forth-83, Forth-94
EXPECT #TIB TIB
  \ Non-standard basic character operations
(.)

\ char-number conversion
  \ Core
# #> #S <# >NUMBER HOLD S>D SIGN
  \ extension words
HOLDS
  \ Forth-79, Forth-83, Forth-93
CONVERT
  \ Non-standard
#>> (NUMBER) (NUMBER?) <<# DIGIT DPL HLD NUMBER <<# <# # #> #>>
#S (NUMBER) (NUMBER?) CONVERT D>F DIGIT DPL F>D HLD HOLD NUMBER
S>D SIGN >NUMBER F>S S>F HOLDS

\ interpreter, wordbook, compiler
  \ Core
>BODY >IN ACCEPT ENVIRONMENT?  EVALUATE HERE QUIT SOURCE
  \ Core extension
ACTION-OF DEFER!  DEFER@ PAD PARSE PARSE-NAME REFILL
RESTORE-INPUT SAVE-INPUT SOURCE-ID
  \ Forth-79
79-STANDARD
  \ Forth-83
<MARK <RESOLVE >MARK >RESOLVE ?BRANCH BRANCH FORTH-83
  \ Forth-79, Forth-83, Forth-94
QUERY
  \ Forth-83, Forth-94
SPAN
  \ Non-standard
) >LINK >NEXT >VIEW ASSERT( ASSERT0( ASSERT1( ASSERT2( ASSERT3(
BODY> CFA COLD L>NAME LINK> N>LINK NAME> VIEW VIEW>

\ booleans
TRUE FALSE

\ numbers
DECIMAL HEX BASE

\ --- Character Matching

[CHAR] ( this-is-not-a-comment
CHAR ( this-is-not-a-comment

\ --- Abort

ABORT ABORT" message"

ABORT" message
not-continued-message

\ --- Numbers

123 #123 -123 #-123
$123 $cafe $-123 $-cafe
%1010 %-1010

123. #123. -123. #-123.
$123. $cafe. $-123. $-cafe.
%1010. %-1010.

1E 1.E 1.E0 +1.23E-1 -1.23E+1

\ --- Includes

INCLUDE filename
REQUIRE filename

\ --- Optional block word set

BLK BLOCK BUFFER FLUSH LOAD SAVE-BUFFERS UPDATE
  \ Extension
EMPTY-BUFFERS LIST REFILL SCR THRU
  \ Non-standard
+LOAD +THRU --> BLOCK-INCLUDED BLOCK-OFFSET BLOCK-POSITION
EMPTY-BUFFER GET-BLOCK-FID OPEN-BLOCKS SAVE-BUFFER UPDATED? USE

\ --- Optional double-number word set

D>S
2CONSTANT 2LITERAL 2VARIABLE
D. D.R
D+ D- D0= D2* D2/ D= DABS DMAX DMIN DNEGATE D0< D< M+ M*/
  \ Extension
2VALUE
DU<
2ROT
  \ Non-standard
D0<= D0<> D0> D0>= D<= D<> D> D>= DU<= DU> DU>=
2-ROT 2NIP 2RDROP 2TUCK

\ --- Optional exception word set

CATCH THROW
  \ Extension
ABORT ABORT" ..."

\ --- Optional Facility word set

AT-XY KEY?  PAGE
  \ Extension
EKEY EKEY>CHAR EKEY>FKEY EKEY?  EMIT?  K-ALT-MASK K-CTRL-MASK
K-DELETE K-DOWN K-END K-F1 K-F10 K-F11 K-F12 K-F2 K-F3 K-F4 K-F5
K-F6 K-F7 K-F8 K-F9 K-HOME K-INSERT K-LEFT K-NEXT K-PRIOR
K-RIGHT K-SHIFT-MASK K-UP
+FIELD BEGIN-STRUCTURE CFIELD: END-STRUCTURE FIELD:
MS TIME&DATE

\ --- Optional File-Access word set

( ...)
BIN R/O R/W W/O
CLOSE-FILE CREATE-FILE DELETE-FILE FILE-POSITION FILE-SIZE
INCLUDE-FILE INCLUDED OPEN-FILE READ-FILE READ-LINE
REPOSITION-FILE RESIZE-FILE SOURCE-ID WRITE-FILE WRITE-LINE
S" ..." 
  \ Extension
FILE-STATUS FLUSH-FILE REFILL RENAME-FILE REQUIRED
INCLUDE filename
REQUIRE filename
S\" ..."
  \ Non-standard
EMIT-FILE KEY-FILE KEY?-FILE SLURP-FID SLURP-FILE STDERR STDIN
STDOUT
FLOAD filename
NEEDS filename

\ --- Optional Floating-Point word set

1E 1.E 1.E0 +1.23E-1 -1.23E+1
>FLOAT D>F F>D
FALIGN FALIGNED FLOAT+ FLOATS
FCONSTANT FLITERAL FVARIABLE
FDROP FDUP FOVER FROT FSWAP
REPRESENT
F! F@
F* F+ F- F/ F0< F0= F< FLOOR FMAX FMIN FNEGATE FROUND
FDEPTH
  \ Extension
F>S S>F
DFALIGN DFALIGNED DFLOAT+ DFLOATS SFALIGN SFALIGNED SFLOAT+
SFLOATS
DFFIELD: FFIELD: FVALUE SFFIELD:
F. FE. FS. PRECISION SET-PRECISION
DF! DF@ SF! SF@
F** FABS FACOS FACOSH FALOG FASIN FASINH FATAN FATAN2 FATANH
FCOS FCOSH FEXP FEXPM1 FLN FLNP1 FLOG FSIN FSINCOS FSINH FSQRT
FTAN FTANH FTRUNC F~
  \ Non-standard
1/F F2* F2/ F~ABS F~REL
FNIP FTUCK

\ --- Optional Locals word set

(LOCAL)
  \ Extension
{: a b c | d e f -- g h i :}
LOCALS| a b c |
  \ Non-standard
{ a b c }

\ Optional Memory-Allocation word set

ALLOCATE FREE RESIZE

\ --- Optional Programming-Tools wordset

.S ? DUMP SEE WORDS
  \ Extension
;CODE ASSEMBLER CODE END-CODE
AHEAD CS-PICK CS-ROLL
NAME>COMPILE NAME>INTERPRET NAME>STRING SYNONYM
TRAVERSE-WORDLIST [DEFINED] [ELSE] [IF] [THEN] [UNDEFINED]
BYE FORGET
N>R NR>
EDITOR
  \ Non-standard
FLUSH-ICACHE
PRINTDEBUGDATA PRINTDEBUGLINE ~~
[+LOOP] [?DO] [AGAIN] [BEGIN] [DO] [ENDIF] [IFDEF] [IFUNDEF]
[LOOP] [NEXT] [REPEAT] [UNTIL] [WHILE]

\ --- Optional Search-Order word set

DEFINITIONS FIND FORTH-WORDLIST GET-CURRENT GET-ORDER
SEARCH-WORDLIST SET-CURRENT SET-ORDER WORDLIST
  \ Extension
ALSO FORTH ONLY ORDER PREVIOUS
  \ Forth-79, Forth-83
CONTEXT CURRENT VOCABULARY
  \ Non-standard
#VOCS ROOT SEAL VOCS

\ --- Optional String word set

-TRAILING /STRING BLANK CMOVE CMOVE> COMPARE SEARCH SLITERAL
  \ Extension
REPLACES SUBSTITUTE UNESCAPE

\ --- Optional Extended-Character word set

XCHAR+
X-SIZE XC-SIZE XEMIT XKEY XKEY?
XC,
XC!+ XC!+? XC@+
  \ Extension
+X/STRING XCHAR- X\STRING-
CHAR ( EKEY>XCHAR X-WIDTH XC-WIDTH [CHAR] ( 
XHOLD
-TRAILING-GARBAGE
( Plugin Functionality Tests)

\ --- Matchit

\ TEST:    Matchit functionality for colon definitions.
\ EXECUTE: With the cursor intially positioned on the ":" or ":NONAME" in the
\   examples, execute the Vim % command repeatedly.
\ RESULT:  The cursor should cycle between ":" or ":NONAME",
\   "EXIT" and ";" ignoring those words contained in strings and
\   comments.

: MATCHIT ( -- )
    S" skip these : :NONAME EXIT ;"
    EXIT
    ( and these : :NONAME EXIT ;)
;

:NONAME ( -- )
    S" skip these : :NONAME EXIT ;"
    EXIT
    ( and these : :NONAME EXIT ;)
;

\ TEST:    Matchit functionality for conditional compilation
\   words.
\ EXECUTE: With the cursor intially positioned on the "[IF]" in the
\   example, execute the Vim % command repeatedly.
\ RESULT:  The cursor should cycle between "[IF]", "[ELSE]" and
\   "[THEN]" ignoring those words contained in strings and
\   comments.

DEBUG [IF]
    S" skip these [ELSE] [THEN]"
[ELSE]
    ( skip these [ELSE] [THEN])
[THEN]

\ --- Definition Search ('define' and 'include' options)

\ TEST:    Test definition searches within a single file.
\ EXECUTE: With the cursor on MEANING in line 2 execute the Vim
\   [d command.
\ RESULT:  The definition of MEANING should be displayed in the
\   message line.

42 CONSTANT MEANING
MEANING \ <- execute [d here

\ TEST:    Test definition searches in included files.
\ EXECUTE: With the cursor on the word ELSEWHERE1 execute the
\   Vim [<C-D> command.
\ RESULT:  A jump to the definition of ELSEWHERE1 is executed.

INCLUDE input/external1.fs ELSEWHERE1 \ <- execute [<C-D> here

\ TEST:    Test definition searches in included files without a
\   filename extension.
\ EXECUTE: With the cursor on the word ELSEWHERE2 execute the
\   Vim [<C-D> command.
\ RESULT:  A jump to the definition of ELSEWHERE2 is executed.

INCLUDE input/external2 ELSEWHERE2 \ <- execute [<C-D> here

\ vim: set tw=64:
