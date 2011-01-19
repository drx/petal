{
module Syntax.Parser where

import Syntax.Lexer
import Syntax.Term
}

%name parse program1

%tokentype { Token }
%error     { parseError }

%token
	Assign          { (_,TkAssign) 	}
	Colon		{ (_,TkColon)	}
	Delimiter	{ (_,TkDelimiter)   }
	If		{ (_,TkIf)     	}
	Int		{ (_,TkInt $$) 	}
	Jump		{ (_,TkJump)	}
	Name		{ (_,TkName $$)	}
	Register	{ (_,TkRegister $$) }
	Plus            { (_,TkPlus)  	}

%left Delimiter
%left If Jump
%left Assign
%left Plus
%%                              

value :: { Value }
value:		Int				{ Int $1 }
     		| Name				{ VLabel $1 }
		| Register			{ Register $1 }

instruction :: { Instruction }
instruction:	Register Assign value		{ Assign $1 $3 }
	   	| Register Assign Register Plus value { AssignPlus $1 $3 $5 }
		| If Register Jump value	{ IfJump $2 $4 }
		| Name Colon			{ Label $1 }

instruction1 :: { [Instruction] }
instruction1:	instruction			{ [$1] }
		| instructionLabel		{ $1 }

instructionLabel :: { [Instruction] }
instructionLabel: Name Colon instruction	{ [Label $1, $3] }

instructionSeq :: { [Instruction] }
instructionSeq: Jump value			{ [Jump $2] }
	        | instruction1 Delimiter instructionSeq { $1++$3 }

program	:: { Program }	
program:	instructionSeq 			{ [$1]  }
       		| instructionSeq Delimiter	{ [$1]  }
       		| instructionSeq Delimiter program { $1:$3 }

program1 :: { Program }
program1:	program				{ $1 }
		| Delimiter program		{ $2 }

{

parseError :: [Token] -> a
parseError (((line,col),t):xs) = error $ "Parse error at line " ++ (show line) ++ ", column " ++ (show col)
parseError [] = error "Parse error at the end"

}
