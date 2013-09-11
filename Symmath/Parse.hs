module Symmath.Parse where

import Symmath.Terms

data PTerm = Val Val
	   | UnOp UnOp PTerm
	   | BinOp BinOp PTerm PTerm
	   | Undef
	   deriving Eq

data Val = Num Double
	 | Var Char
	 | Const Const
	 deriving Eq

-- Const -> Constant.

data UnOp = Neg
    deriving Eq

data BinOp = Sum
	   | Diff
	   | Prod
	   | Frac
	   | Power
	   deriving Eq

pTermToSym :: PTerm -> SymTerm
