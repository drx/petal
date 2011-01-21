module Typechecker where

import Syntax.Term
import Data.List


psi :: Psi -> String -> Type
psi p s = case lookup s p of
		Just tp -> tp
		Nothing -> error $ "No such code labelled " ++ s

gamma :: Gamma -> Int -> Type
gamma g n = case lookup n g of
		Just tp -> tp
		Nothing -> error $ "No such register r" ++ (show n)

gammasubst :: Gamma -> Int -> Type -> Gamma
gammasubst g r t = (r,t):(filter (\x -> fst x /= r) g)

tcv :: Psi -> Gamma -> Value -> Type
tcv p g (Int n) = TInt
tcv p g (Label l) = psi p l
tcv p g (Register n) = gamma g n

tci :: Psi -> Gamma -> Instruction -> (Gamma, Gamma)
tci p g (Assign r v) = (g, gammasubst g r (tcv p g v))
tci p g (AssignPlus r1 r2 v) = if (tcv p g (Register r1) == TInt)
					then if (tcv p g v == TInt)
						then (g, gammasubst g r1 TInt)
						else terror $ (show v) ++ " not Int"
					else terror $ "r" ++ (show r2) ++ " not Int"
tci p g (IfJump r v) = if tcv p g (Register r) == TInt
				then if tcv p g v == TCode g
					then (g, g)
					else terror $ (show v) ++ " not the type of code("++(show g)++")"
				else terror $ "r" ++ (show r) ++ " not Int"

tcr :: Psi -> RegisterFile -> [Int] -> Gamma
--tcr p rf rs = 

tc (h, rf, i) = let 	p = tch h
			rs = registersh h
			g = tcr p rf rs in
				(p, g, tcis p i)

terror :: String -> a
terror s = error $ "type error: " ++ s
