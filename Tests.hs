module Tests where

import Prelude hiding (lex)
import Syntax.Lexer
import Syntax.Parser
import Syntax.Term
import Interpreter

tal1Test =  "start: r1 = mem[r2 + 5] \n\
            \mem[r2 + 5] = r2 \n\
            \r3 = malloc 3 \n\
            \commit r4 \n\
            \salloc 6 \n\
            \sfree 2 \n\
            \jump exit"

tal0Test =  "start: r1 = 4 \n\
        \jump exit\n"

tal0Test1 =     "start: \n\ 
        \r1 = 4\n\n\
        \r0 = 1\n\ 
        \jump s2\n\

        \trololol: r33 = r1 + 1\n\
        \ jump exit\n\ 

        \s2: if r0 jump trololol\n\ 
        \r12 = 4\n\
        \jump exit;comment trololol\n"

tal0Test2 = "start: \n\ 
        \r1 = 4\n\n\
        \r0 = 0\n\ 
        \jump s2\n\

        \trololol: r33 = r1 + 1\n\
        \ jump exit\n\ 

        \s2: if r0 jump trololol\n\ 
        \r12 = 4\n\ 
        \jump exit;comment trololol\n"

tal0Test3 =     "prod: r3 = 0; res = 0\n\ 
        \r1 = 5\n\
        \r2 = 7\n\
        \jump loop\n\n\

        \loop: if r1 jump done; if a == 0 goto done\n\
        \r3 = r2 + r3; res = res + b\n\
        \r1 = r1 + -1; a = a - 1\n\
        \jump loop\n\n\

        \done:\n\
        \jump exit\n\n"
