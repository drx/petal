module Interpreter where

import Prelude hiding (lex)
import Syntax.Term
import Syntax.Lexer
import Syntax.Parser
import Data.List
import Debug.Trace

maxstack :: Int
maxstack = 4096

rep :: String -> IO ()
rep = interpret . statify . parse . lex

interpret :: State -> IO ()
interpret s@(heap, rf, i) = case step s of
    Just s' -> interpret s'
    Nothing -> putStrLn $   "Finished successfully\n\
                            \heap: " ++ (show heap) ++ "\n\
                            \registerfile: " ++ (show rf) ++ "\n" 

uniqueLabel :: Heap -> String
uniqueLabel heap = head $ (labels 0)\\ (map fst heap) where
    labels :: Int -> [String]
    labels n = ["l" ++ (show n)] ++ (labels (n+1))

rhat :: RegisterFile -> Value -> Value
rhat rf (Int n) = Int n
rhat rf (Label l) = Label l
rhat rf (Register n) = r rf n
rhat rf (UPointer r) = (UPointer r)

r :: RegisterFile -> Int -> Value
r rf n = case lookup n rf of
    Just v -> v
    Nothing -> error $ "No value for register " ++ (show n)

h :: RegisterFile -> Heap -> Value -> HeapValue
h rf heap v = case lookup (getLabel $ rhat rf v) heap of
    Just i -> i
    Nothing -> error $ "No code under label " ++ (show v)

substreg :: RegisterFile -> Int -> Value -> RegisterFile
substreg rf r1 v = (r1,v):(filter (\x -> (fst x) /= r1) rf)

substheap :: Heap -> String -> HeapValue -> Heap
substheap heap l v = (l,v):(filter (\x -> (fst x) /= l) heap)

replace :: [a] -> a -> Int -> [a]
replace a v n = let (f,s) = splitAt n a in (init f) ++ (v:s)

step :: State -> Maybe State
step (heap, rf, iss) = case iss of
    Seq s ((Assign r1 v):is) jv rs t -> case rhat rf v of
        UPointer h  -> error $ "Illegal unique pointer access"
        _           -> Just (heap, substreg rf r1 v, Seq s is jv rs t)
    
    Seq s ((Malloc r1 n):is) jv rs t-> Just (heap, substreg rf r1 (UPointer $ Tup (replicate n (Int 0))),Seq s is jv rs t)

    Seq s ((Commit r1):is) jv rs t| isStackPointer r1 -> error $ "Can't commit a stack pointer"
                                  | otherwise         -> case rhat rf (Register r1) of
                                                            UPointer h  -> let l = uniqueLabel heap 
                                                                            in Just ((l,h):heap, substreg rf r1 (Label l), Seq s is jv rs t)
                                                            _           -> error $ "Can't commit something else than unique pointer"
    Seq s ((Load r1 r2 n):is) jv rs t -> case r rf r2 of
        UPointer h -> case h of
            HeapSeq _ -> error "Loading data from code label"
            Tup tp -> Just (heap, substreg rf r1 (tp !! (n - 1)), Seq s is jv rs t)
        Label l -> case  h rf heap (Label l) of
            HeapSeq _ -> error "Loading data from codo label"
            Tup tp -> Just (heap, substreg rf r1 (tp !! (n - 1)), Seq s is jv rs t)

    Seq s ((Save r1 r2 n):is) jv rs t -> case r rf r2 of
        UPointer h -> case h of
            HeapSeq _ -> error "Saving data under code label"
            Tup tp -> case (r rf r1) of
                UPointer _ -> error "Saving data under unique pointer"
                v -> Just (heap, substreg rf r2 (UPointer (Tup (replace tp v n))), Seq s is jv rs t)
        Label l -> case  h rf heap (Label l) of
            HeapSeq _ -> error "Saving data under codo label"
            Tup tp ->  case (r rf r1) of
                UPointer _ -> error "Saving data under unique pointer"
                v -> Just (substheap heap l (Tup tp), rf, Seq s is jv rs t)
        v -> error $ "Some error " ++ (show v)

    Seq s ((Salloc n):is) jv rs t -> case r rf 0 of
        UPointer h -> case h of
            HeapSeq _ -> error "Stack pointer pointing to a code label"
            Tup tp  | (length tp) + n > maxstack -> error "Stack overflow"
                    | otherwise -> Just (heap, substreg rf 0 (UPointer $ Tup $ (replicate n (Int 0)) ++ tp), Seq s is jv rs t)
        _ -> error "Stack pointer containing something else than unique pointer"

    Seq s ((Sfree n):is) jv rs t -> case r rf 0 of
        UPointer h -> case h of
            HeapSeq _ -> error "Stack pointer pointing to a code label"
            Tup tp -> Just (heap, substreg rf 0 (UPointer $ Tup $ (drop n tp)), Seq s is jv rs t)
        _ -> error "Stack pointer containing something else than unique pointer"
        

    Seq s ((AssignPlus r1 r2 v):is) jv rs t -> Just (heap,
                                                substreg rf r1 (Int ((getValue  $ r rf r2) + (getValue $ rhat rf v))),
                                                Seq s is jv rs t)

    Seq s ((IfJump r1 v):is) jv rs t -> case getValue $ r rf r1 of
        0 ->    case v of
                Label l | l == exit -> Nothing
                _ -> case h rf heap (rhat rf v) of
                    HeapSeq i -> Just (heap, rf, i)
                    Tup _ -> error $ "Jumping to a data label" ++ (show v)

        n ->    Just (heap, rf, Seq s is jv rs t)

    Seq s [] jv rs t -> case jv of
        Label l | l == exit -> Nothing
        _ -> case  h rf heap (rhat rf jv) of
            HeapSeq i -> Just (heap, rf, i)
            Tup _ -> error $ "Jumping to a data label " ++ (show jv)

    
    is ->   error $ "Stuck term:\n" ++ 
            "\theap: " ++ (show heap) ++ "\n" ++ 
            "\tregisterfile: " ++ (show rf) ++ "\n" ++ 
            "\tinstructions: " ++ (show is) ++ "\n"
