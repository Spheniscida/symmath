module Symmath.Eval where

import Symmath.Terms
import Symmath.Constants
import Symmath.Util

import qualified Data.Map.Strict as M

import Data.Maybe
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Except

type VarBind = M.Map Char Double

evalTerm :: SymTerm -> Either String Double
evalTerm = flip evalTermP $ M.empty

----------------------------------------------------------------------------------
-- evalTermP uses an association list (carried by a reader monad) for variables
----------------------------------------------------------------------------------

type EvalT = ReaderT VarBind (ExceptT String (Identity)) Double

evalTermP :: SymTerm -> VarBind -> Either String Double
evalTermP t l = runIdentity (runExceptT (runReaderT (evalP t) l))

evalP :: SymTerm -> EvalT
evalP (Variable v) = do
    bindings <- ask
    let val = M.lookup v bindings
    if val == Nothing
        then throwError $ "Unbound variable: " ++ [v]
        else return (fromJust val)
evalP (Constant c) = return . constToNumber $ c
evalP (Number n) = return n
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
evalP (Abs t) = evalP t >>= \et -> if et < 0
                                   then return $ (-1) * et
                                   else return et
evalP (Root b e) = evalP (Power b (rec e))
evalP t = throwError $ "Could not evaluate partial term: " ++ show t ++ "; this is probably a bug."

rEvalUnary :: (Double -> Double) -> SymTerm -> EvalT
rEvalUnary f t = do
    et <- evalP t
    return . f $ et

rEvalBinary :: (Double -> Double -> Double) -> SymTerm -> SymTerm -> EvalT
rEvalBinary op t1 t2 = do
    et1 <- evalP t1
    et2 <- evalP t2
    return $ et1 `op` et2

-- Utilities

-- Is only used when it's guaranteed that the argument is not Nothing

getTrigFun :: Trigo -> (Double -> Double)
getTrigFun t = case t of
                Sin -> sin
                Cos -> cos
                Tan -> tan
                Sinh -> sinh
                Cosh -> cosh
                Tanh -> tanh
                Arcsin -> asin
                Arccos -> acos
                Arctan -> atan
                Arsinh -> asinh
                Arcosh -> acosh
                Artanh -> atanh
