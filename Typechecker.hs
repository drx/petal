module Typechecker where

import Prelude hiding (lex)
import Syntax.Term
import Syntax.Lexer
import Syntax.Parser
import Data.List

lookupErr :: (Eq a) => (a -> String) -> [(a,b)] -> a -> b
lookupErr erf assoc k = case lookup k assoc of
                Just v -> v
                Nothing -> error $ erf k

psiLookup :: Psi -> String -> Type
psiLookup = lookupErr $ \l -> "Block does not exist: " ++ l

gammaLookup :: Gamma -> Int -> Type
gammaLookup = lookupErr $ \n -> "Register does not exist: r" ++ show n

heapLookup :: Heap -> String -> HeapValue
heapLookup = lookupErr $ \l -> "Block does not exist: " ++ l

gammasubst :: Gamma -> Int -> Type -> Gamma
gammasubst g r t = sort $ (r,t):(filter (\x -> fst x /= r) g)

psify :: Program -> Psi
psify [] = []
psify ((Seq l _ _ _ t):is) = (l,t):psify is

rtp = tc . parse . lex

tc :: Program -> Bool
tc p = all (\(l,i) -> tcm (heap, [], getHeapSequence i) psi (getGamma (getAscription $ getHeapSequence i))) heap
    where
        (heap, _, _) = statify p
        psi = psify p

tcm :: State -> Psi -> Gamma -> Bool
tcm (h, rf, i) psi gamma = and [tch h psi, tcr psi rf gamma, tciseq psi i (TCode gamma)]

tch :: Heap -> Psi -> Bool
tch h psi = all (\(l,t) -> tciseq psi (getHeapSequence $ heapLookup h l) t && null (ftv t)) psi

tcr :: Psi -> RegisterFile -> Gamma -> Bool
tcr psi rf gamma = all (\(n,v) -> tcv psi v $ gammaLookup gamma n) rf

tciseq :: Psi -> InstructionSequence -> Type -> Bool
tciseq psi i (TCode gamma) =
    if null (getCode i) then
        tcop psi gamma (getJump i) (TCode gamma)
    else
        let gamma2 = tii psi (head $ getCode i) gamma
         in tciseq psi (i { getCode = tail (getCode i) }) (TCode gamma2)

tciseq psi i (TForall a t) =
    tciseq psi i t

tcop :: Psi -> Gamma -> Value -> Type -> Bool
tcop psi gamma v t = t == tiop psi gamma v

tcv :: Psi -> Value -> Type -> Bool
tcv psi v t = t == tiv psi v

tiv :: Psi -> Value -> Type
tiv psi v = case v of 
    Int _ -> TInt
    Label l -> psiLookup psi l

tiop :: Psi -> Gamma -> Value -> Type
tiop psi gamma v = case v of
    Register r -> gammaLookup gamma r
            
    _ -> tiv psi v

tii :: Psi -> Instruction -> Gamma -> Gamma
tii psi ins gamma = case ins of
    Assign d v -> gammasubst gamma d (tiop psi gamma v)
    AssignPlus d s v | (tcop psi gamma (Register s) TInt) && (tcop psi gamma v TInt) -> gammasubst gamma d TInt
    IfJump s v | (tcop psi gamma (Register s) TInt) && (tcop psi gamma v (TCode gamma)) -> gamma
