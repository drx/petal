module Syntax.Term where

import Data.List

data Value =    Int { getValue :: Int }
                | Label { getLabel :: String }
                | Register { getNumber :: Int }
                deriving (Eq)

instance Show Value where
        show (Int n) = (show n)
        show (Label l) = l
        show (Register n) = "r" ++ (show n)

data Instruction =      Assign Int Value
                        | AssignPlus Int Int Value
                        | IfJump Int Value
                        | Jump Value
                        deriving (Show, Eq)

type Heap = [(String, InstructionSequence)]
type RegisterFile = [(Int, Value)]
type State = (Heap, RegisterFile, InstructionSequence)

type Gamma = [(Int, Type)]
type Psi = [(String, Type)]
data Type = TInt |TCode {getGamma :: Gamma} | TVar String | TForall String Type deriving (Show, Eq)

data InstructionSequence = Seq {getName :: String, getCode :: [Instruction], getJump :: Value, getRegisters :: [Int] } deriving (Show, Eq)
type Program = [InstructionSequence]

exit = "exit"

ftv :: Type -> [String]
ftv (TInt) = []
ftv (TCode g) = concat (map (ftv.snd) g)
ftv (TVar s) = [s]
ftv (TForall s t) = (ftv t) \\ [s]

registersv :: Value -> [Int]
registersv (Int n) = []
registersv (Label l) = []
registersv (Register n) = [n]

registers :: [Instruction] -> [Int]
registers ((Assign n v):is) = n:(registersv v) ++ (registers is)
registers ((AssignPlus n1 n2 v):is) = [n1,n2] ++ (registersv v) ++ (registers is)
registers ((IfJump n v):is) = n:(registersv v) ++ (registers is)
registers ((Jump v):is) = (registersv v) ++ (registers is)
registers [] = []
