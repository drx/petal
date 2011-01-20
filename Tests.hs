module Tests where

import Syntax.Lexer
import Syntax.Parser
import Syntax.Term
import Interpreter

someTest = 	"r1 = 4\njump 4"
someTest1 = 	"r1 = 4\n\njump 4\nr33 = r2 + costam\njump hello\n if r0 jump r0 \n jump bye;comment trololol\n"
someTest2 = 	"hello: r1 = 4\n\njump 4\nr33 = r2 + costam\njump hello\n if r0 jump r0 \n bye: jump bye;comment trololol\n"
		
