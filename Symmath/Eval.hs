module Symmath.Eval where

import Symmath.Terms
import Symmath.Constants

import Data.Maybe
import Control.Monad

mEvalUnary :: (Double -> Double) -> SymTerm -> Maybe Double
mEvalUnary f t1 = evalTerm t1 >>= \et1 -> Just $ f et1

mEvalBinary :: (Double -> Double -> Double) -> SymTerm -> SymTerm -> Maybe Double
mEvalBinary f t1 t2 = evalTerm t1 >>= \et1 -> evalTerm t2 >>= \et2 -> Just $ f et1 et2

evalTerm :: SymTerm -> Maybe Double
evalTerm (Variable _) = Nothing
evalTerm (Constant c) = Just . constToNumber $ c
evalTerm (Number n) = Just n
evalTerm (Negative n) = mEvalUnary ((-1)*) n
evalTerm (Sum t1 t2) = mEvalBinary (+) t1 t2 
evalTerm (Product t1 t2) = mEvalBinary (*) t1 t2
evalTerm (Fraction t1 t2) = mEvalBinary (/) t1 t2
evalTerm (Power t1 t2) = mEvalBinary (**) t1 t2
evalTerm (Exp t1) = mEvalUnary (euler**) t1
