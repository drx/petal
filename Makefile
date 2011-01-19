all: lexer parser

lexer: Syntax/Lexer.hs

Syntax/Lexer.hs: Syntax/Lexer.x
	alex Syntax/Lexer.x

parser: Syntax/Parser.hs

Syntax/Parser.hs: Syntax/Parser.y
	happy -iSyntax/grammar Syntax/Parser.y
