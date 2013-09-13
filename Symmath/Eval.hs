module Symmath.Eval where

import Symmath.Terms
import Symmath.Constants
import Symmath.Assoc

import Data.Maybe
import Control.Monad.Reader

evalTerm :: SymTerm -> Maybe Double
evalTerm (Variable _) = Nothing
evalTerm (Constant c) = Just . constToNumber $ c
evalTerm (Number n) = Just n
evalTerm (Sum t1 t2) = mEvalBinary (+) t1 t2
evalTerm (Difference t1 t2) = mEvalBinary (-) t1 t2
evalTerm (Product t1 t2) = mEvalBinary (*) t1 t2
evalTerm (Fraction t1 t2) = mEvalBinary (/) t1 t2
evalTerm (Power t1 t2) = mEvalBinary (**) t1 t2
evalTerm (Exp t1) = mEvalUnary (euler**) t1
evalTerm (Trigo f t1) = mEvalUnary (getTrigFun f) t1
evalTerm (Ln t1) = mEvalUnary log t1
evalTerm (Log t1 t2) = mEvalBinary logBase t1 t2
evalTerm (Signum t1) = mEvalUnary signum t1
evalTerm (Abs t) = evalTerm t >>= \et -> if et < 0 then Just $ et * (-1) else Just et

-- Generic may-fail evaluation
mEvalUnary :: (Double -> Double) -> SymTerm -> Maybe Double
mEvalUnary f t1 = evalTerm t1 >>= \et1 -> Just $ f et1

mEvalBinary :: (Double -> Double -> Double) -> SymTerm -> SymTerm -> Maybe Double
mEvalBinary f t1 t2 = evalTerm t1 >>= \et1 -> evalTerm t2 >>= \et2 -> Just $ f et1 et2

----------------------------------------------------------------------------------
-- evalTermP uses an association list (carried by a reader monad) for variables
----------------------------------------------------------------------------------

evalTermP :: SymTerm -> VarBind -> Maybe Double
evalTermP t l = runReader (evalP t) l

evalP :: SymTerm -> Reader VarBind (Maybe Double)
evalP (Variable v) = do
    bindings <- ask
    let val = lookupVar bindings v
    if val == Nothing
    then return Nothing
    else return val
evalP (Constant c) = return . Just . constToNumber $ c
evalP (Number n) = return . Just $ n
evalP (Sum t1 t2) = rEvalBinary (+) t1 t2
evalP (Difference t1 t2) = rEvalBinary (-) t1 t2
evalP (Product t1 t2) = rEvalBinary (*) t1 t2
evalP (Fraction t1 t2) = rEvalBinary (/) t1 t2
evalP (Power t1 t2) = rEvalBinary (**) t1 t2
evalP (Exp t) = rEvalUnary exp t
evalP (Trigo f t) = rEvalUnary (getTrigFun f) t
evalP (Ln t) = rEvalUnary log t
evalP (Log t1 t2) = rEvalBinary logBase t1 t2
evalP (Signum t) = rEvalUnary signum t
evalP (Abs t) = evalP t >>= \et -> if Nothing /= et
                                   then if fromMb et < 0
                                        then return . Just $ (-1) * fromMb et
                                        else return et
                                   else return Nothing

-- Like mEval{U,Bi}nary, but with the Reader monad

rEvalUnary :: (Double -> Double) -> SymTerm -> Reader VarBind (Maybe Double)
rEvalUnary f t = do
    et <- evalP t
    if Nothing /= et
    then return . Just $ f . fromMb $ et
    else return Nothing

rEvalBinary :: (Double -> Double -> Double) -> SymTerm -> SymTerm -> Reader VarBind (Maybe Double)
rEvalBinary op t1 t2 = do
    et1 <- evalP t1
    et2 <- evalP t2
    if Nothing /= et1 && Nothing /= et2
    then return . Just $ (fromMb et1) `op` (fromMb et2)
    else return Nothing

-- Utilities

-- Is only used when it's guaranteed that the argument is not Nothing
fromMb :: Maybe a -> a
fromMb (Just a) = a

getTrigFun :: Trigo -> (Double -> Double)
getTrigFun t = case t of
                Sin -> sin
                Cos -> cos
                Tan -> tan
