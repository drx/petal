module Syntax.Term where

data Value = 	Int Int 
		| VLabel String
		| Register Int
		deriving (Show, Eq)

data Instruction = 	Assign Int Value
			| AssignPlus Int Int Value
			| IfJump Int Value
			| Jump Value
			| Label String
			deriving (Show, Eq)

type Program = [[Instruction]]
