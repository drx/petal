module Interpreter where

import Prelude hiding (lex)
import Syntax.Term
import Syntax.Lexer
import Syntax.Parser
import Debug.Trace

rep :: String -> IO ()
rep = interpret . statify . parse . lex

statify :: Program -> State
statify (i:is) = (statify1 (i:is), [], i) where
                        statify1 :: Program -> Heap
                        statify1 (i@(Seq l iss jv rs):is) = (l,i):(statify1 is)
                        statify1 [] = []

interpret :: State -> IO ()
interpret s@(heap, rf, i) = case step s of
                                Just s' -> interpret s'
                                Nothing -> putStrLn $ "Finished" ++ "\n" ++
--                                              "\theap: " ++ (show heap) ++ "\n" ++
                                                "\tregisterfile: " ++ (show rf) ++ "\n" -- ++
--                                              "\tinstructions: " ++ (show i) ++ "\n"

rhat :: RegisterFile -> Value -> Value
rhat rf (Int n) = Int n
rhat rf (Label l) = Label l
rhat rf (Register n) = r rf n

r :: RegisterFile -> Int -> Value
r rf n = case lookup n rf of
                Just v -> v
                Nothing -> error $ "No value for register " ++ (show n)

h :: RegisterFile -> Heap -> Value -> InstructionSequence
h rf heap v = case lookup (getLabel $ rhat rf v) heap of
                        Just i -> i
                        Nothing -> error $ "No code under label " ++ (show v)

substreg :: RegisterFile -> Int -> Value -> RegisterFile
substreg rf r1 v = (r1,v):(filter (\x -> (fst x) /= r1) rf)

step :: State -> Maybe State
step (heap, rf, iss) = case iss of
                        Seq s ((Assign r1 v):is) jv rs ->               Just (heap,
                                                                        substreg rf r1 v,
                                                                        Seq s is jv rs)
                        Seq s ((AssignPlus r1 r2 v):is) jv rs ->        Just (heap,
                                                                        substreg rf r1 (Int ((getValue$r rf r2)+(getValue$rhat rf v))),
                                                                        Seq s is jv rs)
                        Seq s ((IfJump r1 v):is) jv rs -> case getValue $ r rf r1 of
                                                                0 ->    case v of
                                                                        Label l | l == exit -> Nothing
                                                                        _ -> Just       (heap,
                                                                                        rf,
                                                                                        h rf heap (rhat rf v))
                                                                n ->    Just (heap,
                                                                        rf,
                                                                        Seq s is jv rs)
                        Seq s [] jv rs ->       case jv of
                                                        Label l | l == exit -> Nothing
                                                        _ -> Just       (heap,
                                                                        rf,
                                                                        h rf heap (rhat rf jv))
                        
                        is ->   error $ "Stuck term:\n" ++ 
                                "\theap: " ++ (show heap) ++ "\n" ++ 
                                "\tregisterfile: " ++ (show rf) ++ "\n" ++ 
                                "\tinstructions: " ++ (show is) ++ "\n"




