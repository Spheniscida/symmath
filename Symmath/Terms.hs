module Symmath.Terms where

import Data.Char (toLower)
import Symmath.Constants

-- Data types

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

data Constant = Euler | Pi | Phi deriving (Eq, Show)

data Trigo = Sin | Cos | Tan deriving (Eq, Show)

-- Instances

instance Show SymTerm where
    show (Number n) = show n
    show (Variable v) = [v]
    show (Constant c) = show c
    show (Product (Number (-1)) term2) = "(-" ++ (show term2) ++ ")"
    show (Product term1 term2) = (show term1) ++ " * " ++ (show term2)
    show (Sum term1 (Product (Number (-1)) term2)) = '(' : (show term1) ++ " - " ++ (show term2) ++ ")"
    show (Sum term1 term2) = '(' : (show term1) ++ " + " ++ (show term2) ++ ")"
    show (Difference term1 term2) = '(' : (show term1) ++ " - " ++ (show term2) ++ ")"
    show (Fraction term1 term2) = '(' : (show term1) ++ " / " ++ (show term2) ++ ")"
    show (Power term1 term2) = '(' : (show term1) ++ ")^(" ++ (show term2) ++ ")"
    show (Exp term1) = "exp(" ++ show term1 ++ ")"
    show (Ln term1) = "ln(" ++ show term1 ++ ")"
    show (Log base term) = "log(" ++ show base ++ "," ++ show term ++ ")"
    show (Abs term) = '|' : show term ++ "|"
    show (Signum term) = "sgn(" ++ show term ++ ")"
    show (Trigo trigo term1) = (map toLower $ show trigo) ++ "(" ++ show term1 ++ ")"

instance Num SymTerm where
    (+) = Sum
    (*) = Product
    (-) = Difference
    negate = Product (Number (-1))
    abs = Abs
    fromInteger = Number . fromIntegral
    signum = Signum

instance Fractional SymTerm where
    t1 / t2 = Fraction t1 t2
    recip t = Fraction (Number 1) t
    fromRational = Number . fromRational

-- Helper
constToNumber :: Constant -> Double
constToNumber Euler = euler
constToNumber Pi = pi
constToNumber Phi = phi
