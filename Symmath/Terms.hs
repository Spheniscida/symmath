module Symmath.Terms where

import Symmath.Constants

-- Data types

data SymTerm = Number Double
             | Variable Char
             | Constant Constant
--             | Negative SymTerm
             | Product SymTerm SymTerm
             | Difference SymTerm SymTerm
             | Sum SymTerm SymTerm
             | Fraction SymTerm SymTerm
             | Power SymTerm SymTerm
             | Exp SymTerm
             | Trigo Trigo SymTerm
--             | Root SymTerm SymTerm
            deriving Eq

data Constant = Euler | Pi | Phi deriving (Eq, Show)

data Trigo = Sin | Cos | Tan deriving Eq

-- Instances

instance Show SymTerm where
    show (Number n) = show n
    show (Variable v) = [v]
    show (Constant c) = show c
--    Show (-1 * x) as (-x)
    show (Product (Number (-1)) term2) = "(-" ++ (show term2) ++ ")"
    show (Product term1 term2) = (show term1) ++ " * " ++ (show term2)
    show (Sum term1 (Product (Number (-1)) term2)) = '(' : (show term1) ++ " - " ++ (show term2) ++ ")"
    show (Sum term1 term2) = '(' : (show term1) ++ " + " ++ (show term2) ++ ")"
    show (Difference term1 term2) = '(' : (show term1) ++ " - " ++ (show term2) ++ ")"
    show (Fraction term1 term2) = '(' : (show term1) ++ " / " ++ (show term2) ++ ")"
    show (Power term1 term2) = (show term1) ++ "^(" ++ (show term2) ++ ")"
    show (Exp term1) = "exp(" ++ show term1 ++ ")"

constToNumber :: Constant -> Double
constToNumber Euler = euler
constToNumber Pi = pi
constToNumber Phi = phi
