module Symmath.Simplify where

import Symmath.Terms

-- Recombine a term

recombine :: SymTerm -> SymTerm
recombine (Sum a b) = Sum b a
recombine (Product a b) = Product b a
recombine (Difference (Number a) (Number b))  | b < 0 = Sum (Number a) (Negative (Number (-1 * b)))
                                              | otherwise = Sum (Number a) (Negative (Number b))

-- Simplify terms.

simplify :: SymTerm -> Maybe SymTerm
simplify (Sum (Number n1) (Number n2)) = Just . Number $ n1 + n2

simplify (Product (Number n1) (Number n2)) = Just . Number $ n1 * n2
simplify (Product (Power b1 e1) (Power b2 e2)) | b1 == b2 = Just $ Power b1 (Sum e1 e2)
                                               | otherwise = Just (Product (Power b1 e1) (Power b2 e2))

simplify (Difference (Number n1) (Number n2)) | (n1 - n2) < 0 = Just . Negative . Number $ (-1) * (n1-n2)
                                              | otherwise = Just . Number $ (n1-n2)

simplify (Fraction (Number n1) (Number n2)) = Just . Number $ n1 / n2
simplify (Fraction e d) = Just $ Product (e) (Power d (Negative (Number 1)))

simplify (Root p (Power b e)) | p == e = Just b
                              | otherwise = Just $ Power (b) (Fraction e p)
simplify (Root p t) = Just $ Power (t) (Fraction (Number 1) p)

simplify a = Just a
