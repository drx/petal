module Syntax.Term where

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
