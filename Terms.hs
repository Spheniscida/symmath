module Symmath.Terms where

data SymTerm = Number Double
             | Variable Char
             | Constant Char
             | Negative SymTerm
             | Product SymTerm SymTerm
             | Sum SymTerm SymTerm
             | Fraction SymTerm SymTerm
             | Power SymTerm SymTerm
             | Exp SymTerm
            deriving Eq

instance Show SymTerm where
    show (Number n) = show n
    show (Variable c) = [c]
    show (Constant c) = [c]
    show (Negative term) = '-' : show term
    show (Product term1 term2) = (show term1) ++ " * " ++ (show term2)
    show (Sum term1 term2) = '(' : (show term1) ++ " + " ++ (show term2) ++ ")"
    show (Fraction term1 term2) = '(' : (show term1) ++ " / " ++ (show term2) ++ ")"
    show (Power term1 term2) = (show term1) ++ "^(" ++ (show term2) ++ ")"
    show (Exp term1) = "e^(" ++ show term1 ++ ")"
