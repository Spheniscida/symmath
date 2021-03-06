module Symmath.Simplify where

import Data.List

import Symmath.Util
import Symmath.Terms
import Symmath.Eval

-- simplify terms with simplifyOnce until the simplified version is the same as the one from the last simplification step
simplify :: SymTerm -> SymTerm
simplify t = if t == simplifyNext
             then t
             else simplify (simplifyNext)
    where simplifyNext = simplifyOnce t

-- simplify terms (one simplification step)

simplifyOnce :: SymTerm ->  SymTerm
simplifyOnce s@(Sum _ _) = simplifySum s
simplifyOnce p@(Product _ _) = simplifyProd p
simplifyOnce d@(Difference _ _) = simplifyDiff d
simplifyOnce f@(Fraction _ _) = simplifyFrac f
simplifyOnce p@(Power _ _) = simplifyPow p
simplifyOnce l@(Ln t) = simplifyLn l
simplifyOnce l@(Log t1 t2) = simplifyLog l
simplifyOnce a@(Abs _) = simplifyAbs a
simplifyOnce s@(Signum t) = Signum $ simplifyOnce t
simplifyOnce e@(Exp _) = simplifyExp e
simplifyOnce r@(Root _ _) = simplifyRoot r
simplifyOnce u@(UndefP d t) = UndefP d $ simplifyOnce t
simplifyOnce a = a

-- Special cases

simplifySum :: SymTerm -> SymTerm
simplifySum s@(Sum t1 t2) = cleanSum s

-- Products
simplifyProd :: SymTerm -> SymTerm
-- Plain numbers and terms
-- 0 * x = 0
simplifyProd (Product (Number 0) _term) = Number 0
-- x * 0 = 0
simplifyProd (Product _term (Number 0)) = Number 0
simplifyProd (Product (Number (-1)) (Product (Number (-1)) t)) = t
-- a * b => c (c == a * b)
simplifyProd (Product (Number n1) (Number n2)) = Number $ n1 * n2
simplifyProd p@(Product t1 t2) = cleanProduct $ p

-- Differences
simplifyDiff :: SymTerm -> SymTerm
-- Numbers
simplifyDiff (Difference (Number n1) (Number n2)) = Number $ n1 - n2
simplifyDiff (Difference t1 t2) = Sum (t1) (listToSum . map (Product (Number (-1))) . diffSumToSumList $ t2)

-- Fractions
simplifyFrac :: SymTerm -> SymTerm
simplifyFrac (Fraction t1 t2) | t1 == t2 = Number 1
                              | t2 == Number 1 = t1
                              | t1 == Number 0 = Number 0
simplifyFrac (Fraction (Number n1) (Number n2)) | isIntegral n1 && isIntegral n2 = Fraction
                                                                                    (Number (n1 / (fromInteger (gcd (round n1) (round n2)))))
                                                                                    (Number (n2 / (fromInteger (gcd (round n1) (round n2)))))
                                                | otherwise = Number $ n1 / n2
simplifyFrac (Fraction e d) = Product (Power e (Number 1)) (Power d (Number (-1)))

-- Powers
simplifyPow :: SymTerm -> SymTerm
simplifyPow (Power (Number n1) (Number n2)) = Number $ n1**n2
-- (a^b)^c = a^(b*c)
simplifyPow (Power (Power b1 e1) e2) = (Power b1 (Product e1 e2)) -- Abs!?
-- (euler^a)^b = euler^(a*b)
simplifyPow (Power (Exp e1) e2) = (Exp (Product e1 e2))
-- x^0 = 1
simplifyPow (Power b (Number 0)) = Number 1
-- x^1 = x
simplifyPow (Power b (Number 1)) = b
-- e^x = exp(x)
simplifyPow (Power (Constant Euler) t) = Exp t
-- 10^(log(10,y)) = y
simplifyPow (Power b1 (Log b2 t)) | b1 == b2 = t
simplifyPow (Power b1 p@(Product _ _)) | length isAnyLogBase > 0 = let ((Log b2 e2):ts) = isAnyLogBase in Power (e2) (listToProd ((prodToList p) \\ [Log b2 e2]))
    where isAnyLogBase = filter (hasBase b1) . filter isLog . sortProductList . prodToList $ p
          isLog t = case t of
                        (Log _ _) -> True
                        _ -> False
          hasBase b l = case l of
                        (Log b1 e) -> b == b1
                        _ -> False
simplifyPow (Power t1 t2) = Power (simplify t1) (simplify t2)

simplifyAbs :: SymTerm -> SymTerm
simplifyAbs (Abs (Number n)) = Number $ abs n
simplifyAbs (Abs (Product (Number n) t)) = Product (Number $ abs n) (Abs t)
-- abs(a * b) = abs(a) * abs(b)
simplifyAbs (Abs (Product t1 t2)) = Product (Abs t1) (Abs t2)
-- abs(a / b) = abs(a) / abs(b)
simplifyAbs (Abs (Fraction t1 t2)) = Fraction (Abs t1) (Abs t2)
-- abs(a^b) = abs(a)^b
simplifyAbs (Abs (Power b e)) = (Power (Abs b) e)
simplifyAbs (Abs t) = Abs $ simplify t

simplifyExp :: SymTerm -> SymTerm
-- euler^0 = 1
simplifyExp (Exp (Number 0)) = Number 1
-- euler^1 = euler
simplifyExp (Exp (Number 1)) = Constant Euler
-- euler^(ln(x) * y) = x^y. Only this clause; products are sorted so ln's go to the front
simplifyExp (Exp (Product (Ln t1) t2)) = Power t1 t2
simplifyExp (Exp t) = Exp $ simplify t

simplifyLn :: SymTerm -> SymTerm
simplifyLn (Ln (Number n)) = Number $ log n
simplifyLn (Ln (Exp t)) = t
simplifyLn (Ln (Constant Euler)) = Number 1
simplifyLn (Ln t) = Ln (simplify t)

simplifyLog :: SymTerm -> SymTerm
simplifyLog (Log (Number n1) (Number n2)) = Number $ logBase n1 n2
simplifyLog (Log b e) | b == e = Number 1
simplifyLog (Log t1 (Power t2 t3)) | t1 == t2 = t3
simplifyLog (Log t1 t2) = Log (simplify t1) (simplify t2)

simplifyRoot :: SymTerm -> SymTerm
simplifyRoot (Root t1 t2) = Power t2 (rec t1)

---------------------------------------------------------------------
-- List-based simplification algorithms for sums and products. ------
---------------------------------------------------------------------
-- Tidy up products
cleanProduct :: SymTerm -> SymTerm
cleanProduct p@(Product _ _) = listToProd . map simplify . sortProductList . prodListToCommonExps . prodListToPowers . map simplify . prodToList $ p
    where prodlist = if (Number 0) `elem` prodToList p
                     then []
                     else prodToList p

cleanSum :: SymTerm -> SymTerm
cleanSum s@(Sum _ _) = comFac . listToSum . map (foldr1 consolidSum) . groupBy sumGroupable . sortSumList . map simplify . sumToList $ s

-- Converts a product tree into a list (representing the flat structure of multiplications): (x*y) * ((a*b) * z) = x*y*a*b*z
prodToList :: SymTerm -> [SymTerm]
prodToList (Product t1 t2) = prodToList t1 ++ prodToList t2
prodToList p@(Power b (Number n)) = map (flip Power (Number n)) . prodToList $ b
prodToList (Exp t) = [Power (Constant Euler) t]
prodToList (Number 1) = []
prodToList t = [t]

listToProd :: [SymTerm] -> SymTerm
listToProd [] = Number 1
listToProd [t] = t
listToProd ((Number 1):ts) = listToProd ts
listToProd (t:ts) = Product t (listToProd ts)

sumToList :: SymTerm -> [SymTerm]
sumToList (Sum t1 t2) = sumToList t1 ++ sumToList t2
sumToList (Number 0) = []
sumToList t = [t]

listToSum :: [SymTerm] -> SymTerm
listToSum [] = Number 0
listToSum [t] = t
listToSum ((Number 0):ts) = listToSum ts
listToSum (t:ts) = Sum t (listToSum ts)

diffSumToSumList :: SymTerm -> [SymTerm]
diffSumToSumList (Sum t1 t2) = diffSumToSumList t1 ++ diffSumToSumList t2
diffSumToSumList (Difference t1 t2) = diffSumToSumList t1 ++ (map (Product (Number (-1))) $ diffSumToSumList t2)
diffSumToSumList t = [t]

--

-- Data.List.intersect doesn't do what we need here
prodListIntersect :: [SymTerm] -> [SymTerm] -> [SymTerm]
--prodListIntersect ((Number n):xs) ys = prodListIntersect xs ys
prodListIntersect (x:xs) ys = if x `elem` ys
                              then x:(prodListIntersect xs $ delete x ys)
                              else prodListIntersect xs ys
prodListIntersect _ _ = []

-- Converts a product of many terms into a product with the same terms transformed to powers

prodListToPowers :: [SymTerm] -> [SymTerm]
prodListToPowers = map sameFacToPower . groupBy prodGroupable . sortProductList

prodListToCommonExps :: [SymTerm] -> [SymTerm]
prodListToCommonExps = map sameExpToProd . groupBy prodExpGroupable . sortProductExpList

sameFacToPower :: [SymTerm] -> SymTerm
sameFacToPower [t] = t
sameFacToPower a@(x:xs) | all (==x) xs = Power x (Number . fromIntegral . length $ a)
                        | all (sameBase x) xs = foldr1 sumExponent a
                        | otherwise = listToProd a

sameBase :: SymTerm -> SymTerm -> Bool
sameBase (Number n1) (Number n2) = True
sameBase (Power b1 _) (Power b2 _) = b1 == b2
sameBase a (Power b _) = a == b
sameBase (Power b _) a = a == b
sameBase a b = a == b

sumExponent :: SymTerm -> SymTerm -> SymTerm
sumExponent (Power b1 e1) (Power b2 e2) | b1 == b2 = Power b1 (Sum e1 e2)
sumExponent t (Power b e) | t == b = Power b (Sum (Number 1) e)
sumExponent (Power b e) t | t == b = Power b (Sum (Number 1) e)
sumExponent t1 t2 = simplify $ Product t1 t2

--

sameExpToProd :: [SymTerm] -> SymTerm
sameExpToProd [t] = t
sameExpToProd ts@((Power b e):xs) = if comfac /= []
                                    then Power (inParens) (listToProd comfac)
                                    else inParens
    where comfac = foldr (\(Power b e) a -> prodListIntersect a (prodToList e)) (prodToList e) xs
          inParens = listToProd $ map (\(Power b e) -> let remainfac = listToProd (prodToList e \\ comfac) in
                                                       if remainfac == Number 1
                                                       then b
                                                       else Power b remainfac) ts

--            New term   Accumulator
consolidSum :: SymTerm -> SymTerm -> SymTerm
consolidSum (Number n1) (Number n2) = Number $ n1 + n2
consolidSum t1 t2 | t1 == t2 = Product (Number 2) t1
consolidSum t1 (Product t2 t3) | t1 == t3 = Product (Sum t2 (Number 1)) t1
                               | t1 == t2 = Product (Sum t3 (Number 1)) t1
consolidSum (Product t1 t2) t3 | t2 == t3 = Product (Sum t1 (Number 1)) t3
                               | t1 == t3 = Product (Sum t2 (Number 1)) t3
consolidSum (Product (Number n1) t1) (Product (Number n2) t2) | t1 == t2 = Product (Number $ n1 + n2) t1
consolidSum t1 t2 = Sum t1 t2

expandPower :: SymTerm -> [SymTerm]
expandPower p@(Power t (Number n)) | isIntegral n && n > 0 = replicate (round n) t
                                   | isIntegral n && n < 0 = replicate (abs . round $ n) (Power t (Number (-1)))
                                   | otherwise = [p]
expandPower x = [x]

comFac :: SymTerm -> SymTerm
comFac (Number n) = Number n -- Avoiding foldr1 exceptions
comFac t = let is = foldr1 intersect . map prodToList . sumToList $ t
      in if is /= []
         then Product (listToProd is) (listToSum . map listToProd . map (\\is) . map prodToList . sumToList $ t)
         else t

-- Comparison/sort
sortProductList :: [SymTerm] -> [SymTerm]
sortProductList = sortBy prodTermCompare

sortProductExpList :: [SymTerm] -> [SymTerm]
sortProductExpList = sortBy prodExpTermCompare

sortSumList :: [SymTerm] -> [SymTerm]
sortSumList = sortBy sumTermCompare

prodTermCompare :: SymTerm -> SymTerm -> Ordering
prodTermCompare (Number n1) (Number n2) = n1 `compare` n2
prodTermCompare (Variable v1) (Variable v2) = v1 `compare` v2
prodTermCompare (Number n1) (Variable v1) = LT
prodTermCompare (Variable v1) (Number n1) = GT
prodTermCompare (Number n) term = LT
prodTermCompare term (Number n) = GT
prodTermCompare (Power b1 _) (Power b2 _) = b1 `prodTermCompare` b2
prodTermCompare (Ln a) (Ln b) = a `prodTermCompare` b
prodTermCompare (Ln _) _ = LT
prodTermCompare _ (Ln _) = GT
prodTermCompare (Log b1 e1) (Log b2 e2) = e1 `prodTermCompare` e2
prodTermCompare (Power b1 _) t | b1 == t = GT
                               | otherwise = b1 `prodTermCompare` t
prodTermCompare t (Power b2 _) | b2 == t = LT
                               | otherwise = t `prodTermCompare` b2
prodTermCompare (Variable _) t = LT
prodTermCompare t (Variable _) = GT
prodTermCompare (Abs t1) t2 = t1 `prodTermCompare` t2
prodTermCompare t1 (Abs t2) = t1 `prodTermCompare` t2
prodTermCompare _ _ = EQ

prodExpTermCompare :: SymTerm -> SymTerm -> Ordering
prodExpTermCompare (Number n1) (Number n2) = n1 `compare` n2
prodExpTermCompare (Variable v1) (Variable v2) = v1 `compare` v2
prodExpTermCompare (Number n1) (Variable v1) = LT
prodExpTermCompare (Variable v1) (Number n1) = GT
prodExpTermCompare (Power b1 e1) (Power b2 e2) = e1 `prodExpTermCompare` e2
prodExpTermCompare (Ln a) (Ln b) = a `prodExpTermCompare` b
prodExpTermCompare (Ln _) _ = LT
prodExpTermCompare _ (Ln _) = GT
prodExpTermCompare (Power b1 _) t | b1 == t = LT
                                  | otherwise = b1 `prodExpTermCompare` t
prodExpTermCompare t (Power b2 _) | b2 == t = GT
                                  | otherwise = t `prodExpTermCompare` b2
prodExpTermCompare (Variable _) t = GT
prodExpTermCompare (Abs t1) t2 = t1 `prodExpTermCompare` t2
prodExpTermCompare t1 (Abs t2) = t1 `prodExpTermCompare` t2
prodExpTermCompare _ _ = EQ


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

-- Grouping predicates
--

prodGroupable :: SymTerm -> SymTerm -> Bool
prodGroupable (Number _) (Number _) = True
prodGroupable (Power b1 _) (Power b2 _) = b1 == b2
prodGroupable t (Power b _) = t == b
prodGroupable (Power b _) t = t == b
prodGroupable a b = a == b

prodExpGroupable :: SymTerm -> SymTerm -> Bool
prodExpGroupable (Power b1 p1@(Product _ _)) (Power b2 p2@(Product _ _)) = 0 < (length $ prodListIntersect (prodToList p1) (prodToList p2))
prodExpGroupable (Power b1 e1) (Power b2 p@(Product _ _)) = e1 `elem` prodToList p
prodExpGroupable (Power b1 p@(Product _ _)) (Power b2 e2) = e2 `elem` prodToList p
prodExpGroupable (Power b1 e1) (Power b2 e2) = e1 == e2
prodExpGroupable _ _ = False

sumGroupable :: SymTerm -> SymTerm -> Bool
sumGroupable (Number _) (Number _) = True
sumGroupable (Variable a) (Variable b) = a == b
sumGroupable (Product t1 t2) (Product t3 t4) = t1 == t3 || t1 == t4 || t2 == t3 || t2 == t4
sumGroupable t1 (Product t2 t3) = t1 == t3 || t1 == t2
sumGroupable (Product t1 t2) t3 = t1 == t3 || t2 == t3
sumGroupable _ _ = False

-- Helpers

isNumber :: SymTerm -> Bool
isNumber (Number _) = True
isNumber _ = False
