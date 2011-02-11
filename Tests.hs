module Tests where

import Prelude hiding (lex)
import Syntax.Lexer
import Syntax.Parser
import Syntax.Term
import Interpreter

someTest =  "start: r1 = 4 \n\
        \jump exit\n"

someTest1 =     "start: \n\ 
        \r1 = 4\n\n\
        \r0 = 1\n\ 
        \jump s2\n\

        \trololol: r33 = r1 + 1\n\
        \ jump exit\n\ 

        \s2: if r0 jump trololol\n\ 
        \r12 = 4\n\
        \jump exit;comment trololol\n"

someTest2 = "start: \n\ 
        \r1 = 4\n\n\
        \r0 = 0\n\ 
        \jump s2\n\

        \trololol: r33 = r1 + 1\n\
        \ jump exit\n\ 

        \s2: if r0 jump trololol\n\ 
        \r12 = 4\n\ 
        \jump exit;comment trololol\n"

someTest3 =     "prod: r3 = 0; res = 0\n\ 
        \r1 = 5\n\
        \r2 = 7\n\
        \jump loop\n\n\

        \loop: if r1 jump done; if a == 0 goto done\n\
        \r3 = r2 + r3; res = res + b\n\
        \r1 = r1 + -1; a = a - 1\n\
        \jump loop\n\n\

        \done:\n\
        \jump exit\n\n"
