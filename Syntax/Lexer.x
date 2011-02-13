{
module Syntax.Lexer where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :- 
        \;.*                            ;
        [\ \t\f\v\r]+                   ;
        \n[\n$white]*                   { \p s -> tokenWithPos p TkDelimiter }
        r$digit+                        { \p s -> tokenWithPos p (TkRegister (read (tail s))) }
        \-?$digit+                      { \p s -> tokenWithPos p (TkInt (read s)) }
        if                              { \p s -> tokenWithPos p TkIf }
        jump                            { \p s -> tokenWithPos p TkJump }
        int                             { \p s -> tokenWithPos p TkTInt }
        code                            { \p s -> tokenWithPos p TkTCode }
        mem                             { \p s -> tokenWithPos p TkMem }
        malloc                          { \p s -> tokenWithPos p TkMalloc }
        commit                          { \p s -> tokenWithPos p TkCommit }
        salloc                          { \p s -> tokenWithPos p TkSalloc }
        sfree                           { \p s -> tokenWithPos p TkSfree }
        "["                             { \p s -> tokenWithPos p TkLBracket }
        "]"                             { \p s -> tokenWithPos p TkRBracket }
        "+"                             { \p s -> tokenWithPos p TkPlus }
        "="                             { \p s -> tokenWithPos p TkAssign }
        ":"                             { \p s -> tokenWithPos p TkColon }
        "{"                             { \p s -> tokenWithPos p TkLCBrace }
        "}"                             { \p s -> tokenWithPos p TkRCBrace }
        ","                             { \p s -> tokenWithPos p TkComma }
        "."                             { \p s -> tokenWithPos p TkDot }
        "\"                             { \p s -> tokenWithPos p TkLambda }
        $alpha ($digit | $alpha)*       { \p s -> tokenWithPos p (TkName s) }
{                                                 
data BaseToken = 
         TkAssign
        | TkColon
        | TkDelimiter
        | TkIf
        | TkInt Int
        | TkJump
        | TkPlus
        | TkLCBrace
        | TkRCBrace
        | TkComma
        | TkLambda
        | TkDot
        | TkTInt
        | TkTCode
        | TkMem
        | TkCommit
        | TkSalloc
        | TkSfree
        | TkLBracket
        | TkRBracket
        | TkRegister Int
        | TkName String                     
        deriving (Show, Eq)
          
type Token = ((Int,Int), BaseToken)

tokenWithPos :: AlexPosn -> BaseToken -> Token
tokenWithPos (AlexPn _ line col) t  = ((line,col),t)

lex = alexScanTokens
}
