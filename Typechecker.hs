module Typechecker where

import Syntax.Term
import Data.List

type Gamma = [(Int, Type)]
type Psi = [(String, Type)]
data Type = TInt |TCode Gamma | TVar String | TForall String Type deriving (Show, Eq)

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
tcv p g (VLabel l) = psi p l
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



ftv :: Type -> [String]
ftv (TInt) = []
ftv (TCode g) = concat (map (ftv.snd) g)
ftv (TVar s) = [s]
ftv (TForall s t) = (ftv t) \\ [s]

terror :: String -> a
terror s = error $ "type error: " ++ s
