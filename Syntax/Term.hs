module Syntax.Term where

data Value = 	Int Int 
		| Label String
		| Register Int
		deriving (Show, Eq)

data Instruction = 	Assign Int Value
			| AssignPlus Int Int Value
			| IfJump Int Value
			| Jump Value
			deriving (Show, Eq)

type Program = [[Instruction]]
