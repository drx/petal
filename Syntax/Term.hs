module Syntax.Term where

import Data.List

data Value = 	Int { getValue :: Int }
		| VLabel { getLabel :: String }
		| Register { getNumber :: Int }
		deriving (Show, Eq)

data Instruction = 	Assign Int Value
			| AssignPlus Int Int Value
			| IfJump Int Value
			| Jump Value
			| Label String
			deriving (Show, Eq)

type InstructionSequence = [Instruction]
type Program = [[Instruction]]

type Heap = [(String, InstructionSequence)]
type RegisterFile = [(Int, Value)]
type State = (Heap, RegisterFile, InstructionSequence)

type Gamma = [(Int, Type)]
type Psi = [(String, Type)]
data Type = TInt |TCode {getGamma :: Gamma} | TVar String | TForall String Type deriving (Show, Eq)

ftv :: Type -> [String]
ftv (TInt) = []
ftv (TCode g) = concat (map (ftv.snd) g)
ftv (TVar s) = [s]
ftv (TForall s t) = (ftv t) \\ [s]

registersv :: Value -> [Int]
registersv (Int n) = []
registersv (VLabel l) = []
registersv (Register n) = [n]

registersh :: Heap -> [Int]
registersh h = foldl1 (union) $ map (registers.snd) h

registers :: InstructionSequence -> [Int]
registers ((Assign n v):is) = n:(registersv v) ++ (registers is)
registers ((AssignPlus n1 n2 v):is) = [n1,n2] ++ (registersv v) ++ (registers is)
registers ((IfJump n v):is) = n:(registersv v) ++ (registers is)
registers ((Jump v):is) = (registersv v) ++ (registers is)
registers ((Label s):is) = (registers is)
registers [] = []
