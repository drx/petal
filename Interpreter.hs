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

statify :: Program -> State
statify (i:is) = (statify1 (i:is), [], i) where
    statify1 :: Program -> Heap
    statify1 (i@(Seq l iss jv rs):is) = (l,HeapSeq i):(statify1 is)
    statify1 [] = []

interpret :: State -> IO ()
interpret s@(heap, rf, i) = case step s of
    Just s' -> interpret s'
    Nothing -> putStrLn $ "Finished" ++ "\n" ++
--      "\theap: " ++ (show heap) ++ "\n" ++
        "\tregisterfile: " ++ (show rf) ++ "\n" -- ++
--      "\tinstructions: " ++ (show i) ++ "\n"

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
    Seq s ((Assign r1 v):is) jv rs -> case rhat rf (Register r1) of
        UPointer h  -> error $ "Illegal unique pointer access"
        _           -> Just (heap, substreg rf r1 v, Seq s is jv rs)
    
    Seq s ((Malloc r1 n):is) jv rs -> Just (heap, substreg rf r1 (UPointer $ Tup (replicate n (Int 0))),Seq s is jv rs)

    Seq s ((Commit r1):is) jv rs | isStackPointer r1 -> error $ "Can't commit a stack pointer"
                                 | otherwise         -> case rhat rf (Register r1) of
                                                            UPointer h  -> let l = uniqueLabel heap 
                                                                            in Just ((l,h):heap, substreg rf r1 (Label l), Seq s is jv rs)
                                                            _           -> error $ "Can't commit something else than unique pointer"
    Seq s ((Load r1 r2 n):is) jv rs -> case r rf r2 of
        UPointer h -> case h of
            HeapSeq _ -> error "Loading data from code label"
            Tup t -> Just (heap, substreg rf r1 (t !! n), Seq s is jv rs)
        Label l -> case  h rf heap (Label l) of
            HeapSeq _ -> error "Loading data from codo label"
            Tup t -> Just (heap, substreg rf r1 (t !! n), Seq s is jv rs)

    Seq s ((Save r1 r2 n):is) jv rs -> case r rf r2 of
        UPointer h -> case h of
            HeapSeq _ -> error "Saving data under code label"
            Tup t -> case (r rf r1) of
                UPointer _ -> error "Saving data under unique pointer"
                v -> Just (heap, substreg rf r1 (UPointer (Tup (replace t v n))), Seq s is jv rs)
        Label l -> case  h rf heap (Label l) of
            HeapSeq _ -> error "Saving data under codo label"
            Tup t ->  case (r rf r1) of
                UPointer _ -> error "Saving data under unique pointer"
                v -> Just (substheap heap l (Tup t), rf, Seq s is jv rs)

    Seq s ((Salloc n):is) jv rs -> case r rf 0 of
        UPointer h -> case h of
            HeapSeq _ -> error "Stack pointer pointing to a code label"
            Tup t   | (length t) + n > maxstack -> error "Stack overflow"
                    | otherwise -> Just (heap, substreg rf 0 (UPointer $ Tup $ (replicate n (Int 0)) ++ t), Seq s is jv rs)
        _ -> error "Stack pointer containing something else than unique pointer"

    Seq s ((Sfree n):is) jv rs -> case r rf 0 of
        UPointer h -> case h of
            HeapSeq _ -> error "Stack pointer pointing to a code label"
            Tup t -> Just (heap, substreg rf 0 (UPointer $ Tup $ (drop n t)), Seq s is jv rs)
        _ -> error "Stack pointer containing something else than unique pointer"
        

    Seq s ((AssignPlus r1 r2 v):is) jv rs -> Just (heap,
                                                substreg rf r1 (Int ((getValue  $ r rf r2) + (getValue $ rhat rf v))),
                                                Seq s is jv rs)

    Seq s ((IfJump r1 v):is) jv rs -> case getValue $ r rf r1 of
        0 ->    case v of
                Label l | l == exit -> Nothing
                _ -> case h rf heap (rhat rf v) of
                    HeapSeq i -> Just (heap, rf, i)
                    Tup _ -> error "Jumping to a data label"

        n ->    Just (heap, rf, Seq s is jv rs)

    Seq s [] jv rs -> case jv of
        Label l | l == exit -> Nothing
        _ -> case  h rf heap (rhat rf jv) of
            HeapSeq i -> Just (heap, rf, i)
            Tup _ -> error "Jumping to a data label"

    
    is ->   error $ "Stuck term:\n" ++ 
            "\theap: " ++ (show heap) ++ "\n" ++ 
            "\tregisterfile: " ++ (show rf) ++ "\n" ++ 
            "\tinstructions: " ++ (show is) ++ "\n"
