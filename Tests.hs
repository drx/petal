module Tests where

import Prelude hiding (lex)
import Syntax.Lexer
import Syntax.Parser
import Syntax.Term
import Interpreter
import Typechecker

typecheckTest1 = "loop: code{r1: int, r2: int, r3: int}\n\
            \r3 = r2 + r3\n\
            \r1 = r1 + -1\n\
            \jump loop\n"

tal1Test =  "start: r1 = mem[r2 + 5] \n\
            \mem[r2 + 5] = r2 \n\
            \r3 = malloc 3 \n\
            \commit r4 \n\
            \salloc 6 \n\
            \sfree 2 \n\
            \jump exit"

tal1Test1 = "copy: code{}\n\
            \r1 = malloc 2;\n\
            \r2 = 5;\n\
            \r3 = 3;\n\
            \mem[r1+1] = r2;\n\
            \mem[r1+2] = r3;\n\
            \r4 = mem[r1+1]\n\
            \r5 = mem[r1+2]\n\
            \commit r1;\n\
            \jump exit;"

tal1Test2 = "troll:\n\
            \salloc 2\n\
            \r1 = 5\n\
            \r2 = 7\n\
            \mem[r0+1] = r1\n\
            \mem[r0+2] = r2\n\
            \sfree 1\n\
            \jump exit"

tal1Test3 = "copy: ; {r1:ptr(int,int), r2,r3:int}\n\
            \r2 = malloc 2;\n\
            \r3 = mem[r1+1];\n\
            \mem[r2+1] = r3;\n\
            \r3 = mem[r1+2];\n\
            \mem[r2+1] = r3;\n\
            \commit r2;\n\
            \jump exit;{r1:ptr(int,int), r2:ptr(int,int), r3:int }"

tal1Test4 = "copy: code{}\n\
            \r1 = malloc 2;\n\
            \jump exit;"

tal0Test =  "start: code{}\n\
        \r1 = 4 \n\
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

tal0Test3 = "prod: code{} \n\
            \r3 = 0; res = 0\n\ 
            \r1 = 5\n\
            \r2 = 7\n\
            \jump loop\n\n\

            \loop: if r1 jump done; if a == 0 goto done\n\
            \r3 = r2 + r3; res = res + b\n\
            \r1 = r1 + -1; a = a - 1\n\
            \jump loop\n\n\

            \done:\n\
            \jump exit\n\n"
