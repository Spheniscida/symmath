module Symmath.Eval where

import Symmath.Terms

import Data.Maybe
import Control.Monad

mEvalBinary :: (Double -> Double -> Double) -> SymTerm -> SymTerm -> Maybe Double
mEvalBinary f t1 t2 = evalTerm t1 >>= \et1 -> evalTerm t2 >>= \et2 -> Just $ f et1 et2

evalTerm :: SymTerm -> Maybe Double
evalTerm (Constant _) = Nothing
evalTerm (Variable _) = Nothing
evalTerm (Number n) = Just n
evalTerm (Negative n) = Just $ (-1) * n
evalTerm (Sum t1 t2) = mEvalBinary (+) t1 t2 
evalTerm (Product t1 t2) = mEvalBinary (*) t1 t2
evalTerm (Fraction t1 t2) = mEvalBinary (/) t1 t2
evalTerm (Power t1 t2) = mEvalBinary (^) t1 t2
evalTerm (Exp t1) = 
