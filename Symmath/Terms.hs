module Symmath.Terms where

import Symmath.Constants

-- Data types

data SymTerm = Number Double
             | Variable Char
             | Constant Constant
             | Negative SymTerm
             | Product SymTerm SymTerm
             | Difference SymTerm SymTerm
             | Sum SymTerm SymTerm
             | Fraction SymTerm SymTerm
             | Power SymTerm SymTerm
             | Exp SymTerm
             | Trigo Trigo SymTerm
            deriving Eq

data Constant = Euler | Pi | Tau deriving Eq

data Trigo = Sin | Cos | Tan deriving Eq

-- Instances

instance Show SymTerm where
    show (Number n) = show n
    show (Variable v) = [v]
    show (Constant c) = show c
    show (Negative term) = '-' : show term
    show (Product term1 term2) = (show term1) ++ " * " ++ (show term2)
    show (Sum term1 term2) = '(' : (show term1) ++ " + " ++ (show term2) ++ ")"
    show (Fraction term1 term2) = '(' : (show term1) ++ " / " ++ (show term2) ++ ")"
    show (Power term1 term2) = (show term1) ++ "^(" ++ (show term2) ++ ")"
    show (Exp term1) = "e^(" ++ show term1 ++ ")"

instance Show Constant where
    show Euler = show euler
    show Pi = show pi
    show Tau = show tau

constToNumber :: Constant -> Double
constToNumber Euler = euler
constToNumber Pi = pi
constToNumber Tau = tau
