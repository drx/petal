module Interpreter where

import Syntax.Term

type Heap = [(String, InstructionSequence)]
type RegisterFile = [(Int, Value)]

type State = (Heap, RegisterFile, InstructionSequence)

statify :: Program -> State
statify (i:is) = (statify1 (i:is), [], i) where
			statify1 :: Program -> Heap
			statify1 [] = []
			statify1 (i:is) = (heapify i) ++ (statify1 is) where
				heapify :: InstructionSequence -> Heap
				heapify ((Label l):is) = (l,is):(heapify is)
				heapify (_:is) = (heapify is)
				heapify [] = []

interpret :: State -> IO ()
interpret s@(heap, rf, is) 	| is == [] = putStrLn $ (show rf) ++ " " ++ (show heap)
				| is /= [] = interpret (step s)

rhat :: RegisterFile -> Value -> Value
rhat rf (Int n) = Int n
rhat rf (VLabel l) = VLabel l
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

step :: State -> State
step (heap, rf, [Jump v]) = (heap, rf, h rf heap v)

step (heap, rf, (Assign r1 v):is) = (heap, substreg rf r1 v, is)

step (heap, rf, (AssignPlus r1 r2 v):is) = (heap, substreg rf r1 (Int ((getValue $ r rf r2)+(getValue $ rhat rf v))), is)

step (heap, rf, (IfJump r1 v):is) = case getValue $ r rf r1 of
					0 -> (heap, rf, h rf heap (rhat rf v))
					n -> (heap, rf, is)

step (heap, rf, (Label l):is) = (heap, rf, is)

step (heap, rf, is) = error $ "Stuck term:\nheap: " ++ (show heap) ++ "\nregisterfile: " ++ (show rf) ++ "\ninstructions: " ++ (show is)




