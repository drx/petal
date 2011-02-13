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
        Name            { (_,TkName $$) }
        Register        { (_,TkRegister $$) }
        Plus            { (_,TkPlus)    }
        LCBrace         { (_,TkLCBrace)    }
        RCBrace         { (_,TkRCBrace)    }
        Dot             { (_,TkDot)    }
        Comma           { (_,TkComma)    }
        Lambda          { (_,TkLambda)    }
        TInt            { (_,TkTInt)    }
        TCode           { (_,TkTCode)    }

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

instructionSeq :: { InstructionSequence }
instructionSeq: Name Colon type Delimiter instructions Jump value          { Seq $1 $5 $7 (nub $ (registersv $7)++(registers $5)) $3 }

type :: { Type }
type: TInt { TInt }
      | TCode gamma { TCode $2 }
      | Lambda Name Dot type { TForall $2 $4 }
      | Name { TVar $1 }

gamma :: { Gamma }
gamma: LCBrace registertypes RCBrace { $2 }

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
parseError (((line,col),t):xs) = error $ "Parse error at line " ++ (show line) ++ ", column " ++ (show col)
parseError [] = error "Parse error at the end"

}
