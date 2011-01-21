{

module Syntax.Lexer where

}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :- 
	\;.*				;
	[\ \t\f\v\r]+			;
	\n[\n$white]*			{ \p s -> tokenWithPos p TkDelimiter }
	r$digit+			{ \p s -> tokenWithPos p (TkRegister (read (tail s))) }
    	\-?$digit+			{ \p s -> tokenWithPos p (TkInt (read s)) }
	if				{ \p s -> tokenWithPos p TkIf }
	jump				{ \p s -> tokenWithPos p TkJump }
    	"+"				{ \p s -> tokenWithPos p TkPlus }
	"="				{ \p s -> tokenWithPos p TkAssign }
	":"				{ \p s -> tokenWithPos p TkColon }
	$alpha ($digit | $alpha)* 	{ \p s -> tokenWithPos p (TkName s) }
{                                                 
data BaseToken = 
	 TkAssign
	| TkColon
	| TkDelimiter
	| TkIf
	| TkInt Int
	| TkJump
	| TkPlus
	| TkRegister Int
	| TkName String                     
	deriving (Show, Eq)
          
type Token = ((Int,Int), BaseToken)

tokenWithPos :: AlexPosn -> BaseToken -> Token
tokenWithPos (AlexPn _ line col) t  = ((line,col),t)

lexer = alexScanTokens
}
