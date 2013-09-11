module Symmath.Parse where

import Symmath.Terms hiding (Sum)
import qualified Symmath.Terms as ST (SymTerm(Sum))

data PTerm = Val Val
	   | UnOp UnOp PTerm
	   | BinOp BinOp PTerm PTerm
	   | Undef
	   deriving Eq

data Val = Num Double
	 | Var Char
	 | Const Constant
	 deriving Eq

-- Const -> Constant.

data UnOp = Neg
    deriving Eq

data BinOp = Sum
	   | Diff
	   | Prod
	   | Frac
	   | Pow
	   deriving Eq

mConvertToSym :: (SymTerm -> SymTerm -> SymTerm) -> PTerm -> PTerm -> Maybe SymTerm
mConvertToSym f t1 t2 = pTermToSym t1 >>= \et1 -> pTermToSym t2 >>= \et2 -> Just $ f et1 et2

pTermToSym :: PTerm -> Maybe SymTerm
pTermToSym (Val (Num n)) = Just $ Number n
pTermToSym (Val (Var v)) = Just $ Variable v
pTermToSym (Val (Const c)) = Just $ Constant c

pTermToSym (UnOp Neg t) = pTermToSym t >>= \et -> Just $ Negative et

pTermToSym (BinOp Sum t1 t2)  = mConvertToSym (ST.Sum) t1 t2
pTermToSym (BinOp Diff t1 t2) = mConvertToSym (Difference) t1 t2
pTermToSym (BinOp Prod t1 t2) = mConvertToSym (Product) t1 t2
pTermToSym (BinOp Frac t1 t2) = mConvertToSym (Fraction) t1 t2
pTermToSym (BinOp Pow t1 t2)= mConvertToSym (Power) t1 t2
