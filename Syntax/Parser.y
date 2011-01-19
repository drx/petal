{
module Syntax.Parser where

import Syntax.Lexer
import Syntax.Term
}

%name parse program

%tokentype { Token }
%error     { parseError }

%token
	Assign          { (_,TkAssign) 	}
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
     		| Name				{ Label $1 }
		| Register			{ Register $1 }

instruction :: { Instruction }
instruction:	Register Assign value		{ Assign $1 $3 }
	   	| Register Assign Register Plus value { AssignPlus $1 $3 $5 }
		| If Register Jump value	{ IfJump $2 $4 }

instructionSeq :: { [Instruction] }
instructionSeq: Jump value			{ [Jump $2] }
	        | instruction Delimiter instructionSeq { $1:$3 }

program	:: { Program }	
program:	instructionSeq			{ [$1]  }
       		| instructionSeq program 	{ $1:$2 }
		
{

parseError :: [Token] -> a
parseError (((line,col),t):xs) = error $ "Parse error at line " ++ (show line) ++ ", column " ++ (show col)
parseError [] = error "Parse error at the end"

}
