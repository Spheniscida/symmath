module Symmath.Simplify where

import Data.List

import Symmath.Util
import Symmath.Terms

-- simplify terms with simplifyOnce until the simplified version is the same as the one from the last simplification step
simplify :: SymTerm -> SymTerm
simplify t = let simplifiedT = simplifiedT_f in
                if t == simplifiedT
                then t
                else simplify (simplifyOnce t)
    where simplifiedT_f = simplifyOnce t

-- simplify terms (one simplification step)

simplifyOnce :: SymTerm ->  SymTerm
simplifyOnce s@(Sum _ _) = simplifySum s
simplifyOnce p@(Product _ _) = simplifyProd p
simplifyOnce d@(Difference _ _) = simplifyDiff d
simplifyOnce f@(Fraction _ _) = simplifyFrac f
simplifyOnce p@(Power _ _) = simplifyPow p
simplifyOnce l@(Ln t) = Ln $ simplifyOnce t
simplifyOnce l@(Log t1 t2) = Log (simplifyOnce t1) (simplifyOnce t2)
simplifyOnce a@(Abs _) = simplifyAbs a
simplifyOnce s@(Signum t) = Signum $ simplifyOnce t
simplifyOnce e@(Exp _) = simplifyExp e
simplifyOnce u@(UndefP d t) = UndefP d $ simplifyOnce t
simplifyOnce a = a

-- Special cases

simplifySum :: SymTerm -> SymTerm
-- Numbers
simplifySum (Sum (Number n1) (Number n2)) = Number $ n1 + n2
-- n1 + (-n1) = 0
simplifySum (Sum t1 (Product (Number (-1)) t2)) | t1 == t2 = Number 0
simplifySum (Sum t1@(Product _ _) t2@(Product _ _)) = case prodListIntersectTuple (prodToList t1) (prodToList t2) of
                                                        ([],rest1,rest2) -> Sum (listToProd $ rest1) (listToProd $ rest2)
                                                        (common,rest1,rest2) -> Product (listToProd common) (Sum (Product (Number 1) (listToProd rest1)) (Product (Number 1) (listToProd rest2)))
simplifySum (Sum t1 t2) = cleanSum $ Sum (simplifyOnce t1) (simplifyOnce t2)


-- Products
simplifyProd :: SymTerm -> SymTerm
-- Plain numbers and terms
-- 0 * x = 0
simplifyProd (Product (Number 0) _term) = Number 0
-- x * 0 = 0
simplifyProd (Product _term (Number 0)) = Number 0
-- a * b => c (c == a * b)
simplifyProd (Product (Number n1) (Number n2)) = Number $ n1 * n2
-- Simplify negative powers - this doesn't work with the current algos in cleanProduct so we'll do it here
simplifyProd (Product (Power b e@(Number n)) t) | b == t && n < 0 = Power b (Sum e (Number 1))
simplifyProd (Product t (Power b e@(Number n))) | b == t && n < 0 = Power b (Sum e (Number 1))
simplifyProd (Product (Power b1 e1@(Number n1)) (Power b2 e2@(Number n2))) | b1 == b2 && (n1 < 0 || n2 < 0) = Power b1 (Sum e1 e2)
simplifyProd (Product t1 t2) = cleanProduct $ Product (simplifyOnce t1) (simplifyOnce t2)

-- Differences
simplifyDiff :: SymTerm -> SymTerm
-- Numbers
simplifyDiff (Difference (Number n1) (Number n2)) = Number $ n1 - n2
simplifyDiff (Difference t1 t2) = Sum (t1) (Product (Number (-1)) t2)

-- Fractions
simplifyFrac :: SymTerm -> SymTerm
simplifyFrac (Fraction t1 t2) | t1 == t2 = Number 1
simplifyFrac (Fraction (Number n1) (Number n2)) | isIntegral n1 && isIntegral n2 = Fraction
                                                                                    (Number (n1 / (fromInteger (gcd (round n1) (round n2)))))
                                                                                    (Number (n2 / (fromInteger (gcd (round n1) (round n2)))))
                                                | otherwise = Number $ n1 / n2
simplifyFrac (Fraction e d) = Product (Power e (Number 1)) (Power d (Number (-1)))

-- Powers
simplifyPow :: SymTerm -> SymTerm
-- (a^b)^c = a^(b*c)
simplifyPow (Power (Power b1 e1) e2) = (Power b1 (Product e1 e2)) -- Abs!?
-- (euler^a)^b = euler^(a*b)
simplifyPow (Power (Exp e1) e2) = (Exp (Product e1 e2))
-- x^0 = 1
simplifyPow (Power b (Number 0)) = Number 1
-- x^1 = x
simplifyPow (Power b (Number 1)) = b
simplifyPow (Power (Constant Euler) t) = Exp t
simplifyPow (Power t1 t2) = Power (simplifyOnce t1) (simplifyOnce t2)

simplifyAbs :: SymTerm -> SymTerm
-- abs(a * b) = abs(a) * abs(b)
simplifyAbs (Abs (Product t1 t2)) = Product (Abs t1) (Abs t2)
-- abs(a / b) = abs(a) / abs(b)
simplifyAbs (Abs (Fraction t1 t2)) = Fraction (Abs t1) (Abs t2)
-- abs(a^b) = abs(a)^b
simplifyAbs (Abs (Power b e)) = (Power (Abs b) e)
simplifyAbs (Abs t) = Abs $ simplifyOnce t

simplifyExp :: SymTerm -> SymTerm
-- euler^0 = 1
simplifyExp (Exp (Number 0)) = Number 1
-- euler^1 = euler
simplifyExp (Exp (Number 1)) = Constant Euler
simplifyExp (Exp t) = Exp $ simplifyOnce t

--------------------------------------------------------------

sortProductList :: [SymTerm] -> [SymTerm]
sortProductList = sortBy prodTermCompare

prodTermCompare :: SymTerm -> SymTerm -> Ordering
prodTermCompare (Number n1) (Number n2) = n1 `compare` n2
prodTermCompare (Variable v1) (Variable v2) = v1 `compare` v2
prodTermCompare (Number n1) (Variable v1) = LT
prodTermCompare (Variable v1) (Number n1) = GT
prodTermCompare (Power b1 _) (Power b2 _) = b1 `prodTermCompare` b2
prodTermCompare (Power b1 _) t | b1 == t = GT
                           | otherwise = b1 `prodTermCompare` t
prodTermCompare t (Power b2 _) | b2 == t = LT
                           | otherwise = t `prodTermCompare` b2
prodTermCompare (Variable _) t = LT
prodTermCompare _ _ = EQ

-- Converts a product tree into a list (representing the flat structure of multiplications): (x*y) * ((a*b) * z) = x*y*a*b*z
prodToList :: SymTerm -> [SymTerm]
prodToList (Product t1 t2) = prodToList t1 ++ prodToList t2
prodToList p@(Power b (Number n)) | isIntegral n && n > 0 = replicate (round n) b
                                  | otherwise = [p]
prodToList (Number 1) = []
prodToList t = [t]

listToProd :: [SymTerm] -> SymTerm
listToProd [] = Number 1
listToProd [t] = t
listToProd (t:ts) = Product t (listToProd ts)

prodListIntersectTuple :: [SymTerm] -> [SymTerm] -> ([SymTerm],[SymTerm],[SymTerm])
prodListIntersectTuple a b = let is = prodListIntersect a b in
                             (is, a \\ is, b \\ is)

-- Data.List.intersect doesn't do what we need here
prodListIntersect :: [SymTerm] -> [SymTerm] -> [SymTerm]
prodListIntersect ((Number n):xs) ys = prodListIntersect xs ys
prodListIntersect (x:xs) ys = if x `elem` ys
                              then x:(prodListIntersect xs $ delete x ys)
                              else prodListIntersect xs ys
prodListIntersect _ _ = []

-- Converts a product of many terms into a product with the same terms transformed to powers

prodListToPowers :: [SymTerm] -> [SymTerm]
prodListToPowers = map sameFacToPower . groupBy prodGroupable . sortProductList

prodGroupable :: SymTerm -> SymTerm -> Bool
prodGroupable (Power b1 _) (Power b2 _) = b1 == b2
prodGroupable t (Power b _) = t == b
prodGroupable (Power b _) t = t == b
prodGroupable a b = a == b

sameFacToPower :: [SymTerm] -> SymTerm
sameFacToPower [t] = t
sameFacToPower a@(x:xs) | all (==x) xs = Power x (Number . fromIntegral . length $ a)
                        | all (sameBase x) xs = foldr1 sumExponent a
                        | otherwise = listToProd a

sameBase :: SymTerm -> SymTerm -> Bool
sameBase (Power b1 _) (Power b2 _) = b1 == b2
sameBase a (Power b _) = a == b
sameBase (Power b _) a = a == b
sameBase a b = a == b

sumExponent :: SymTerm -> SymTerm -> SymTerm
sumExponent (Power b1 e1) (Power b2 e2) | b1 == b2 = Power b1 (Sum e1 e2)
sumExponent t (Power b e) | t == b = Power b (Sum (Number 1) e)
sumExponent (Power b e) t | t == b = Power b (Sum (Number 1) e)
sumExponent t1 t2 = Product t1 t2

-- Same for sums (not as good yet!)

sumToList :: SymTerm -> [SymTerm]
sumToList (Sum t1 t2) = sumToList t1 ++ sumToList t2
sumToList (Number 0) = []
sumToList t = [t]

listToSum :: [SymTerm] -> SymTerm
listToSum [] = Number 0
listToSum [t] = t
listToSum (t:ts) = Sum t (listToSum ts)

sumTermCompare :: SymTerm -> SymTerm -> Ordering
sumTermCompare (Number _) (Number _) = EQ
sumTermCompare (Variable v1) (Variable v2) = v1 `compare` v2
sumTermCompare (Number _) (Variable _) = LT
sumTermCompare (Variable _) (Number _) = GT
sumTermCompare (Variable v1) (Product (Number n) (Variable v2)) = v1 `compare` v2
sumTermCompare (Product (Number n) (Variable v2)) (Variable v1)  = v2 `compare` v1
sumTermCompare (Number _) t = LT
sumTermCompare (Variable _) t = LT
sumTermCompare t (Number _) = GT
sumTermCompare t (Variable _) = GT
sumTermCompare t1 t2 = EQ

sumGroupable :: SymTerm -> SymTerm -> Bool
sumGroupable (Number _) (Number _) = True
sumGroupable (Variable a) (Variable b) = a == b
sumGroupable (Variable a) (Product (Number n) (Variable b)) | a == b = True
sumGroupable (Product (Number n) (Variable b)) (Variable a) | a == b = True
sumGroupable (Product _ _) (Product _ _) = True
sumGroupable _ _ = False

--            New term   Accumulator
consolidSum :: SymTerm -> SymTerm -> SymTerm
consolidSum (Variable v1) (Variable v2) | v1 == v2 = Product (Number 2) (Variable v1)
consolidSum (Variable v1) (Product (Number n) (Variable v2)) | v1 == v2 = Product (Number $ n+1) (Variable v1)
consolidSum (Product (Number n) (Variable v2)) (Variable v1)  | v1 == v2 = Product (Number $ n+1) (Variable v1)
consolidSum (Product (Number n1) t1) (Product (Number n2) t2) | t1 == t2 = Product (Number $ n1 + n2) t1
consolidSum t1 t2 = Sum t1 t2

-- Tidy up products
cleanProduct :: SymTerm -> SymTerm
cleanProduct p@(Product _ _) = listToProd . prodListToPowers {-. sortBy prodTermCompare // sorted in prodListToPowers-} . prodToList $ p

cleanSum :: SymTerm -> SymTerm
cleanSum s@(Sum _ _) = listToSum . map (foldr1 consolidSum) . groupBy sumGroupable . sortBy sumTermCompare . sumToList $ s
