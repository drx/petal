module Syntax.Term where

import Data.List

data Value =    Int { getValue :: Int }
                | Label { getLabel :: String }
                | Register { getNumber :: Int }
                | UPointer { getHeapValue :: HeapValue }
                deriving (Eq)

isStackPointer :: Int -> Bool
isStackPointer  0 = True
isStackPointer _ = False

instance Show Value where
    show (Int n) = (show n)
    show (Label l) = l
    show (Register n) = "r" ++ (show n)
    show (UPointer h) = "uptr(" ++ (show h) ++ ")"

data Instruction =        Assign {  getAssignDestination :: Int,
                                    getAssignedValue :: Value }
                        | AssignPlus {  getAssignPlusDestination :: Int,
                                        getAssignedRegister :: Int,
                                        getAssignedSummand :: Value }
                        | IfJump {  getIfCondition :: Int,
                                    getIfJumpDestination :: Value }
                        | Jump {    getJumpDestination :: Value }
                        | Load {    getLoadDestination :: Int,
                                    getLoadAddress :: Int,
                                    getLoadOffset :: Int }
                        | Save {    getSavedValue :: Int,
                                    getSaveAddress :: Int,
                                    getSaveOffset :: Int }
                        | Malloc {  getMallocDestination :: Int,
                                    getMallocCount :: Int }
                        | Commit {  getCommitRegister :: Int }
                        | Sfree  {  getSfreeCount :: Int }
                        | Salloc {  getSallocCount :: Int }
                        deriving (Eq)

instance Show Instruction where
   show (Assign r v) = "r" ++ (show r) ++ " = " ++ (show v)
   show (AssignPlus r r1 v) = "r" ++ (show r) ++ " = r" ++ (show r1) ++ " + " ++ (show v)
   show (IfJump r v) = "if (r" ++ (show r) ++ " == 0) jump " ++ (show v)
   show (Jump v) = "jump " ++ (show v)
   show (Load r1 r2 n) = "r" ++ (show r1) ++ " = mem[r" ++ (show r2) ++ " + " ++ (show n) ++ "]"
   show (Save r1 r2 n) = "mem[r" ++ (show r2) ++ " + " ++ (show n) ++ "] = r" ++ (show r1)
   show (Malloc r n) = "r" ++ (show r) ++ " = malloc " ++ (show n)
   show (Commit r) = "commit r" ++ (show r)
   show (Sfree n) = "sfree " ++ (show n)
   show (Salloc n) = "salloc " ++ (show n)

type Heap = [(String, HeapValue)]
type RegisterFile = [(Int, Value)]
type State = (Heap, RegisterFile, InstructionSequence)
type Gamma = [(Int, Type)]
type Psi = [(String, Type)]

data Type =   TInt 
            | TCode {getGamma :: Gamma} 
            | TVar String 
            | TForall String Type 
            deriving (Show, Eq)

data HeapValue =      HeapSeq InstructionSequence
                    | Tup [Value]
                    deriving (Show, Eq)

data InstructionSequence = Seq {    getName :: String,
                                    getCode :: [Instruction],
                                    getJump :: Value,
                                    getRegisters :: [Int] } 
                                    deriving (Eq)

instance Show InstructionSequence where
    show (Seq s is v rs) = s ++ "(" ++ (showRegisters rs) ++ "):\n" ++
                            (unlines (map show is)) ++
                            "jump " ++ (show v) where
                                showRegisters (r:rs) | rs /= [] = "r" ++ (show r) ++ ", " ++ (showRegisters rs)
                                                     | otherwise = "r" ++ (show r)

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
registers ((Load r1 r2 n):is) = [r1,r2] ++ (registers is)
registers ((Save r1 r2 n):is) = [r1,r2] ++ (registers is)
registers ((Malloc r1 n):is) = [r1] ++ (registers is)
registers ((Commit r1):is) = [r1] ++ (registers is)
registers ((Salloc n):is) = (registers is)
registers ((Sfree n):is) = (registers is)
registers [] = []
