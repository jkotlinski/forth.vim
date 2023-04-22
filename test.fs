( This file tests Forth syntax highlighting.
  The goal is that it will cover all functionality.
  Please update this file as you make changes. )

\ --- Comments

\ comments are highlighted like this...
( ...or this... )
( ...multi-line comments
  are fine too! )
.( Somehow, this also counts as a comment. )

\ conditional compilation, "discard-it" should be highlighted as a comment
0 [IF] discard-it [ENDIF]
0 [IF] discard-it [ELSE] compile-it [THEN]
FALSE [IF] discard-it [ELSE] compile-it [ENDIF]
FALSE [IF] discard-it [THEN]
1 [IF] compile-it [ENDIF]
1 [IF] compile-it [ELSE] discard-it [THEN]
TRUE [IF] compile-it [ELSE] discard-it [ENDIF]
TRUE [IF] compile-it [THEN]

0 [if]
  discard-it
[else]
  compile-it
  0 [if]
    discard-it
  [else]
    compile-it
    1 [if]
      compile-it
      0 [if]
        discard-it
	1 [if]
	  compile-it
	[else]
	  discard-it
	[then]
      [else]
        compile-it
	0 [if]
	  discard-it
	[else]
	  compile-it
	[then]
      [then]
    [else]
      discard-it
      1 [if]
	compile-it
	0 [if]
	  discard-it
	[else]
	  compile-it
	[then]
      [else]
	discard-it
	0 [if]
	  discard-it
	[else]
	  compile-it
	[then]
      [then]
    [then]
  [then]
[then]

\ --- Strings

\ Verify that strings have their own color.
s" s-quote"
c" c-quote"
s\" s-backslash-quote"
." dot-quote"
foo" custom string matching word"

\ Verify that escaped characters have their own color.
s\" \a\b\e\f\l\m\n\q\r\t\v\z\"\xff\x00\\"

\ Upper-case characters cannot be escaped.
s\" \A\B\E\F\L\M\N\Q\R\T\V\Z\Xff\X00"

\ --- Spell Checking

( activate spell checking by ":set spell"
then, verify that the below "mispelt"
words are highlighted. )
s" mispelt"
c" mispelt"
( mispelt )
\ mispelt
s\" mispelt"

\ --- To Do

\ TODO FIXME XXX

\ --- Ignore Case

dup DUP Dup

\ --- iskeyword

dup-drop \ dup and drop should not be highlighted

\ --- Keywords

\ basic mathematical and logical operators
+ - * / MOD /MOD NEGATE ABS MIN MAX
AND OR XOR NOT LSHIFT RSHIFT INVERT 2* 2/ 1+
1- 2+ 2- 8* UNDER+
M+ */ */MOD M* UM* M*/ UM/MOD FM/MOD SM/REM
D+ D- DNEGATE DABS DMIN DMAX D2* D2/
F+ F- F* F/ FNEGATE FABS FMAX FMIN FLOOR FROUND
F** FSQRT FEXP FEXPM1 FLN FLNP1 FLOG FALOG FSIN
FCOS FSINCOS FTAN FASIN FACOS FATAN FATAN2 FSINH
FCOSH FTANH FASINH FACOSH FATANH F2* F2/ 1/F
F~REL F~ABS F~
0< 0<= 0<> 0= 0> 0>= < <= <> = > >= U< U<=
U> U>= D0< D0<= D0<> D0= D0> D0>= D< D<= D<>
D= D> D>= DU< DU<= DU> DU>= WITHIN ?NEGATE
?DNEGATE

\ various words that take an input and do something with it
. U. .R U.R

\ stack manipulations
DROP NIP DUP OVER TUCK SWAP ROT -ROT ?DUP PICK ROLL
2DROP 2NIP 2DUP 2OVER 2TUCK 2SWAP 2ROT 2-ROT
3DUP 4DUP 5DUP 3DROP 4DROP 5DROP 8DROP 4SWAP 4ROT
4-ROT 4TUCK 8SWAP 8DUP
>R R> R@ RDROP 2>R 2R> 2R@ 2RDROP
4>R 4R> 4R@ 4RDROP
FDROP FNIP FDUP FOVER FTUCK FSWAP FROT

\ stack pointer manipulations
SP@ SP! FP@ FP! RP@ RP! LP@ LP! DEPTH

\ address operations
@ ! +! C@ C! 2@ 2! F@ F! SF@ SF! DF@ DF!
CHARS CHAR+ CELLS CELL+ CELL ALIGN ALIGNED FLOATS
FLOAT+ FLOAT FALIGN FALIGNED SFLOATS SFLOAT+
SFALIGN SFALIGNED DFLOATS DFLOAT+ DFALIGN DFALIGNED
MAXALIGN MAXALIGNED CFALIGN CFALIGNED
ADDRESS-UNIT-BITS ALLOT ALLOCATE
MOVE ERASE FILL BLANK UNUSED

\ conditionals
IF ELSE ENDIF THEN CASE OF ENDOF ENDCASE ?DUP-IF
?DUP-0=-IF AHEAD CS-PICK CS-ROLL CATCH THROW WITHIN
BEGIN WHILE REPEAT UNTIL AGAIN
?DO LOOP I J K +DO U+DO -DO U-DO DO +LOOP -LOOP
UNLOOP LEAVE ?LEAVE EXIT DONE FOR NEXT RECURSE

\ new words
; ;M ;m ;class ;object
CONSTANT 2CONSTANT FCONSTANT VARIABLE 2VARIABLE
FVARIABLE CREATE USER VALUE TO DEFER IS <BUILDS DOES> IMMEDIATE
COMPILE-ONLY COMPILE RESTRICT INTERPRET POSTPONE EXECUTE
LITERAL CREATE-INTERPRET/COMPILE INTERPRETATION>
<INTERPRETATION COMPILATION> <COMPILATION ] LASTXT
COMP' POSTPONE, FIND-NAME NAME>INT NAME?INT NAME>COMP
NAME>STRING STATE C; CVARIABLE BUFFER: MARKER
, 2, F, C, COMPILE,

\ debugging
PRINTDEBUGDATA PRINTDEBUGLINE

\ Assembler
ASSEMBLER CODE END-CODE ;CODE FLUSH-ICACHE C,

\ basic character operations
(.) EXPECT FIND WORD TYPE EMIT KEY
KEY? TIB CR BL COUNT SPACE SPACES

\ char-number conversion
<<# <# # #> #>> #S (NUMBER) (NUMBER?) CONVERT D>F
D>S DIGIT DPL F>D HLD HOLD NUMBER S>D SIGN >NUMBER
F>S S>F HOLDS

\ interpreter, wordbook, compiler
(LOCAL) BYE COLD ABORT >BODY >NEXT >LINK CFA >VIEW HERE
PAD WORDS VIEW VIEW> N>LINK NAME> LINK> L>NAME FORGET
BODY> ASSERT( ASSERT0( ASSERT1( ASSERT2( ASSERT3( )
>IN ACCEPT ENVIRONMENT? EVALUATE QUIT SOURCE ACTION-OF
DEFER! DEFER@ PARSE PARSE-NAME REFILL RESTORE-INPUT
SAVE-INPUT SOURCE-ID

\ vocabularies
ONLY FORTH ALSO ROOT SEAL VOCS ORDER CONTEXT #VOCS
VOCABULARY DEFINITIONS

\ File keywords
R/O R/W W/O BIN
OPEN-FILE CREATE-FILE CLOSE-FILE DELETE-FILE
RENAME-FILE READ-FILE READ-LINE KEY-FILE
KEY?-FILE WRITE-FILE WRITE-LINE EMIT-FILE
FLUSH-FILE FILE-STATUS FILE-POSITION
REPOSITION-FILE FILE-SIZE RESIZE-FILE
SLURP-FILE SLURP-FID STDIN STDOUT STDERR
INCLUDE-FILE INCLUDED REQUIRED
OPEN-BLOCKS USE LOAD --> BLOCK-OFFSET
GET-BLOCK-FID BLOCK-POSITION LIST SCR BLOCK
BUFER EMPTY-BUFFERS EMPTY-BUFFER UPDATE UPDATED?
SAVE-BUFFERS SAVE-BUFFER FLUSH THRU +LOAD +THRU
BLOCK-INCLUDED BLK

\ The optional String word set
-TRAILING /STRING BLANK CMOVE CMOVE> COMPARE
SEARCH SLITERAL REPLACES SUBSTITUTE UNESCAPE

\ booleans
TRUE FALSE

\ numbers
DECIMAL HEX BASE

\ --- Character Matching

[char] ( this-is-not-a-comment
char ( this-is-not-a-comment

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

include filename
require filename
