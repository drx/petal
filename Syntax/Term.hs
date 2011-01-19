module Syntax.Term where

data Value = 	Int Int 
		| Label String
		| Register Int

data Instruction = 	Assign Int Value
			| AssignPlus Int Int Value
			| IfJump Int Value
			| Jump Value

type Program = [[Instruction]]
