{
module Syntax.Parser where

import Prelude hiding (lex)
import Syntax.Lexer
import Syntax.Term
import Data.List
}

%name parse program1

%tokentype { Token }
%error     { parseError }

%token
        Assign          { (_,TkAssign)  }
        Colon           { (_,TkColon)   }
        Delimiter       { (_,TkDelimiter)   }
        If              { (_,TkIf)      }
        Int             { (_,TkInt $$)  }
        Jump            { (_,TkJump)    }
	Mem             { (_,TkMem)     }
	Malloc		{ (_,TkMalloc) }
	Commit          { (_,TkCommit) }
	Salloc          { (_,TkSalloc) }
	Sfree           { (_,TkSfree) }
	LBracket        { (_,TkLBracket) }
	RBracket	{ (_,TkRBracket) }
        Name            { (_,TkName $$) }
        Register        { (_,TkRegister $$) }
        Plus            { (_,TkPlus)    }
        LCBrace         { (_,TkLCBrace)    }
        RCBrace         { (_,TkRCBrace)    }
        LParen         { (_,TkLParen)    }
        RParen         { (_,TkRParen)    }
        Dot             { (_,TkDot)    }
        Comma           { (_,TkComma)    }
        Lambda          { (_,TkLambda)    }
        TInt            { (_,TkTInt)    }
        TCode           { (_,TkTCode)    }
        TPtr           { (_,TkTPtr)    }
        TUPtr           { (_,TkTUPtr)    }

%left Delimiter
%left If Jump
%left Assign
%left Plus
%%                              

value :: { Value }
value:          Int                                                     { Int $1 }
                | Name                                                  { Label $1 }
                | Register                                              { Register $1 }

instruction :: { Instruction }
instruction:    Register Assign value                                   { Assign $1 $3 }
                | Register Assign Register Plus value                   { AssignPlus $1 $3 $5 }
                | If Register Jump value                                { IfJump $2 $4 }
                | Register Assign Mem LBracket Register Plus Int RBracket       { Load $1 $5 $7 }
                | Mem LBracket Register Plus Int RBracket Assign Register       { Save $8 $3 $5 }
                | Register Assign Malloc Int                                    { Malloc $1 $4}
                | Commit Register                                               { Commit $2 }
                | Salloc Int                                                    { Salloc $2 }
                | Sfree Int                                                     { Sfree $2 }

instructionSeq :: { InstructionSequence }
instructionSeq: Name Colon type Delimiter instructions Jump value          { Seq $1 $5 $7 (nub $ (registersv $7)++(registers $5)) $3 }

type :: { Type }
type: TInt { TInt }
      | TCode gamma { TCode $2 }
      | Lambda Name Dot type { TForall $2 $4 }
      | TPtr LParen atype RParen { TPtr $3 }
      | TUPtr LParen atype RParen { TUPtr $3 }
      | Name { TVar $1 }

atype: type { ATValue $1 }
        | type Comma atype { ATAdjacent (ATValue $1) $3 }
        | { ATEmpty }

gamma :: { Gamma }
gamma: 		LCBrace registertypes RCBrace { $2 }
       		| LCBrace RCBrace { [] }

registertypes :: { Gamma }
registertypes: Register Colon type { [($1,$3)] }
               | Register Colon type Comma registertypes { [($1,$3)] ++ $5 }

instructions :: { [Instruction] }
instructions:   instruction Delimiter instructions                      { $1:$3 }
                |                                                       { []    }

program :: { Program }  
program:        instructionSeq                                          { [$1]  }
                | instructionSeq Delimiter                              { [$1]  }       
                | instructionSeq Delimiter program                      { $1:$3 }

program1 :: { Program }
program1:       program                                                 { $1 }
                | Delimiter program                                     { $2 }

{
parseError :: [Token] -> a
parseError (((line,col),t):xs) = error $ "Parse error at line " ++ (show line) ++ ", column " ++ (show col) ++ " (" ++ show t ++ ")"
parseError [] = error "Parse error at the end"
}
