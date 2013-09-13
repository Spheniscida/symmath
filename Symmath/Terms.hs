{-# LANGUAGE GADTs, MultiParamTypeClasses #-}

module Symmath.Terms where


import Data.Char (toLower)
import Symmath.Constants

-- Data types
{-
data SymTerm = Number Double
             | Variable Char
             | Constant Constant
             | Product SymTerm SymTerm
             | Difference SymTerm SymTerm
             | Sum SymTerm SymTerm
             | Fraction SymTerm SymTerm
             | Power SymTerm SymTerm
             | Exp SymTerm
             | Trigo Trigo SymTerm
             | Ln SymTerm
             | Log SymTerm SymTerm
             | Abs SymTerm
             | Signum SymTerm
             | UndefP Double SymTerm
            deriving Eq
-}

data Number = Number Double deriving Eq
data Variable = Variable Char deriving Eq
data Constant = Euler | Pi | Phi deriving Show
data Product where Product :: (SymTerm a, SymTerm b) => a -> b -> Product
data Difference where Difference :: (SymTerm a, SymTerm b) => a -> b -> Difference
data Sum where Sum :: (SymTerm a, SymTerm b) => a -> b -> Sum
data Power where Power :: (SymTerm a, SymTerm b) => a -> b -> Power
data Exp where Exp :: (SymTerm a) => a -> Exp
data Trigo where Trigo :: (SymTerm b) => TrigoFun -> b -> Trigo
data Ln where Ln :: (SymTerm a) => a -> Ln
data Log where Log :: (SymTerm a, SymTerm b) => a -> b -> Log
data Abs where Abs :: (SymTerm a) => a -> Abs
data Signum where Signum :: (SymTerm a) => a -> Signum
data UndefP where UndefP :: (SymTerm b) => Double -> b -> UndefP

data TrigoFun = Sin | Cos | Tan deriving (Eq, Show)


-- Type classes and instances
class (Show a) => SymTerm a where


instance SymTerm Number where

instance SymTerm Variable where

instance SymTerm Constant where

instance SymTerm Product where

instance SymTerm Difference where

instance SymTerm Sum where

instance SymTerm Power where

instance SymTerm Exp where

instance SymTerm Trigo where

instance SymTerm Ln where

instance SymTerm Log where

instance SymTerm Abs where

instance SymTerm Signum where

instance SymTerm UndefP where


simplify :: (SymTerm a, SymTerm b) => a -> b
simplify (Sum (Number n1) (Number n2)) = Number $ n1+n2
simplify (Product (Number n1) (Number n2)) = Number $ n1*n2
simplify t = t

--- Show instances

instance Show Number where
    show (Number n) = show n
instance Show Variable where
    show (Variable v) = [v]
instance Show Product where
    show (Product t1 t2) = '(' : show t1 ++ " * " ++ show t2 ++ ")"
instance Show Difference where
    show (Difference t1 t2) = '(' : show t1 ++ " - " ++ show t2 ++ ")"
instance Show Sum where
    show (Sum t1 t2) = '(' : show t1 ++ " + " ++ show t2 ++ ")"
instance Show Power where
    show (Power t1 t2) = '(' : show t1 ++ ")^(" ++ show t2 ++ ")"
instance Show Exp where
    show (Exp t) = "exp(" ++ show t ++ ")"
instance Show Trigo where
    show (Trigo f t) = (map toLower . show $ f) ++ "(" ++ show t ++ ")"
instance Show Ln where
    show (Ln t) = "ln(" ++ show t ++ ")"
instance Show Log where
    show (Log b t) = "log(" ++ show b ++ "," ++ show t ++ ")"
instance Show Abs where
    show (Abs t) = '|' : show t ++ "|"
instance Show Signum where
    show (Signum t) = "sgn(" ++ show t ++ ")"
instance Show UndefP where
    show (UndefP p t) = "[" ++ show t ++ "| undefined at " ++ show p ++ "]"

