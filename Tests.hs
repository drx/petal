module Tests where

import Prelude hiding (lex, catch)
import Syntax.Lexer
import Syntax.Parser
import Syntax.Term
import Interpreter
import Typechecker
import Control.Exception
import System.Console.ANSI

greenColor = setSGR [SetColor Foreground Vivid Green]
yellowColor = setSGR [SetColor Foreground Vivid Yellow]
redColor = setSGR [SetColor Foreground Vivid Red]
cyanColor = setSGR [SetColor Foreground Vivid Cyan]
resetColor = setSGR [Reset]

data TestType = In | Tc

data Test = Passable String | Unpassable String

getString :: Test -> String
getString (Passable s) = s
getString (Unpassable s) = s

isPassable :: Test -> Bool
isPassable (Passable _) = True
isPassable (Unpassable _) = False

runTcTests :: IO()
runTests fun tp nm rng = mapM_ (\x -> do
    cyanColor
    putStrLn (nm ++ " Test #" ++ show x)
    resetColor
    catch (do 
        fun (getString $ test tp x) 
        if isPassable (test tp x)
            then greenColor >> putStrLn "Pass"
            else redColor >> putStrLn "Fail - should have failed, but passed"
        resetColor) ((\ex -> do if isPassable (test tp x) 
                                    then redColor >> (putStrLn $ "\nFail") >> yellowColor >> putStrLn ("Exception caught: " ++ (show ex))
                                    else greenColor >> (putStrLn $ "\nPass - Controlled failure") >> yellowColor >> (putStrLn $ "Exception caught: " ++ (show ex))
                                resetColor) :: SomeException -> IO ())) rng

runInTests = runTests rep In "Interpreter" [0..12]
runTcTests = runTests retp Tc "Typechecker" [0..1]
runTcInTests = runTests retp In "Typechecker on Interpreter" [0..12]

test Tc 0 = Passable "loop: code{r1: int, r2: int, r3: int}\n\
            \r3 = r2 + r3\n\
            \r1 = r1 + -1\n\
            \jump loop\n"

test Tc 1 = Unpassable "loop: code{r1: code{}, r2: int, r3: int}\n\
            \r3 = r2 + r3\n\
            \r1 = r1 + -1\n\
            \jump loop\n"

test In 0 = Unpassable "start: code{r0: uptr(), r2: uptr(int,int,int,int,int)} \n\
            \r1 = mem[r2 + 5] \n\
            \mem[r2 + 5] = r1 \n\
            \r3 = malloc 3 \n\
            \commit r3 \n\
            \salloc 6 \n\
            \sfree 2 \n\
            \jump exit"

test In 1 = Unpassable "start: code{r0: uptr(int), r2: int} \n\
            \r1 = mem[r2 + 5] \n\
            \mem[r2 + 5] = r1 \n\
            \r3 = malloc 3 \n\
            \commit r3 \n\
            \salloc 6 \n\
            \sfree 2 \n\
            \jump exit"


test In 2 = Passable "troll: code{r0: uptr()}\n\
            \salloc 2\n\
            \r1 = 5\n\
            \r2 = 7\n\
            \mem[r0+1] = r1\n\
            \mem[r0+2] = r2\n\
            \sfree 1\n\
            \jump exit"

test In 3 = Passable "troll: code{r0: ptr()}\n\
            \salloc 2\n\
            \r1 = 5\n\
            \r2 = 7\n\
            \mem[r0+1] = r1\n\
            \mem[r0+2] = r2\n\
            \sfree 1\n\
            \jump exit"

test In 4 = Unpassable "copy: code{r1:ptr(int,int), r2:int,r3:int}\n\
            \r2 = malloc 2;\n\
            \r3 = mem[r1+1];\n\
            \mem[r2+1] = r3;\n\
            \r3 = mem[r1+2];\n\
            \mem[r2+1] = r3;\n\
            \commit r2;\n\
            \jump exit"

test In 5 = Unpassable "copy: code{r1:ptr(int), r2:int,r3:int}\n\
            \r2 = malloc 2;\n\
            \r3 = mem[r1+1];\n\
            \mem[r2+1] = r3;\n\
            \r3 = mem[r1+2];\n\
            \mem[r2+1] = r3;\n\
            \commit r2;\n\
            \jump exit"

test In 6 = Unpassable "copy: code{r1:ptr(int), r2:int}\n\
            \r2 = malloc 2;\n\
            \r3 = mem[r1+1];\n\
            \mem[r2+1] = r3;\n\
            \r3 = mem[r1+2];\n\
            \mem[r2+1] = r3;\n\
            \commit r2;\n\
            \jump exit"

test In 7 = Passable "copy: code{}\n\
            \r1 = malloc 2;\n\
            \jump exit;"

test In 8 = Passable "start: code{r1:int}\n\
        \r1 = 4 \n\
        \jump exit\n"

test In 9 = Passable "start: code{r0: int, r1:int, r12:int, r33:int}\n\ 
        \r1 = 4\n\n\
        \r0 = 1\n\ 
        \jump s2\n\

        \trololol: code{r0:int, r1:int, r12:int, r33:int}\n\
        \r33 = r1 + 1\n\
        \ jump exit\n\ 

        \s2: code{r0:int, r1:int, r12:int, r33:int}\n\
        \if r0 jump trololol\n\ 
        \r12 = 4\n\
        \jump exit;comment trololol\n"

test In 10 = Passable "start: code{r0: int, r1:int, r12:int, r33:int}\n\ 
        \r1 = 4\n\n\
        \r0 = 1\n\ 
        \jump s2\n\

        \trololol: code{r0:int, r1:code{}, r12:int, r33:int}\n\
        \r33 = r1 + 1\n\
        \ jump exit\n\ 

        \s2: code{r0:int, r1:int, r12:int, r33:int}\n\
        \if r0 jump trololol\n\ 
        \r12 = 4\n\
        \jump exit;comment trololol\n"

test In 11 = Passable "prod: code{r1:int, r2:int, r3:int} \n\
            \r3 = 0; res = 0\n\ 
            \r1 = 5\n\
            \r2 = 7\n\
            \jump loop\n\n\

            \loop: code{r1:int, r2:int, r3:int}\n\
            \if r1 jump done; if a == 0 goto done\n\
            \r3 = r2 + r3; res = res + b\n\
            \r1 = r1 + -1; a = a - 1\n\
            \jump loop\n\n\

            \done: code{r1:int, r2:int, r3:int}\n\
            \jump exit\n\n"

test In 12 = Passable "copy: code{}\n\
            \r1 = malloc 2;\n\
            \r2 = 5;\n\
            \r3 = 3;\n\
            \mem[r1+1] = r2;\n\
            \mem[r1+2] = r3;\n\
            \r4 = mem[r1+1]\n\
            \r5 = mem[r1+2]\n\
            \commit r1;\n\
            \jump exit;"
