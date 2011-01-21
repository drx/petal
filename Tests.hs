module Tests where

import Syntax.Lexer
import Syntax.Parser
import Syntax.Term
import Interpreter

someTest = 	"r1 = 4\njump 4"
someTest1 = 	"r1 = 4\n\njump 4\nr33 = r2 + costam\njump hello\n if r0 jump r0 \n jump bye;comment trololol\n"
someTest2 = 	"hello: r1 = 4\n\njump 4\nr33 = r2 + costam\njump hello\n if r0 jump r0 \n bye: jump bye;comment trololol\n"
someTest3 = 	"prod: r3 = 0; res = 0\n r1 = 0\n jump loop\n \n loop: if r1 jump done; if a == 0 goto done\n r3 = r2 + r3; res = res + b\n r1 = r1 + -1; a = a - 1 \n jump loop\n \n done:\n jump r4\n \n"
