module Symmath.Derivate where

import Symmath.Terms


----------
deriv :: Var -> SymTerm -> SymTerm

deriv _ (Number   _) = Number 0
deriv _ (Constant _) = Number 0

deriv x (Variable y) | x == y = Number 1
                     | x /= y = Number 0

deriv x (Sum        f g) = deriv x f + deriv x g
deriv x (Difference f g) = deriv x f - deriv x g

deriv x (Product  f g) =  deriv x f * g + f * deriv x g
deriv x (Fraction f g) = (deriv x f * g - f * deriv x g) / g `Power` 2

deriv x t@(Exp a) = deriv x a * t
deriv x   (Ln  a) = deriv x a / a

deriv x (Power a b) = deriv x . Exp $ Ln a * b
deriv x (Log   a b) = deriv x $ Ln b / Ln a

deriv x (Abs a) = deriv x a * Signum a
deriv x (Signum a) = UndefP 0 (Number 0)


deriv x (UndefP p t) = UndefP p $ deriv x t
----------
