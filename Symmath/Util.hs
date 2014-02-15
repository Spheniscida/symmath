module Symmath.Util where

import Data.List

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

-- Checks if a double can be considered to be an integer (i.e., whether it is considerably different from its floor)
isIntegral :: Double -> Bool
isIntegral x = (x - (fromIntegral . floor $ x)) == 0

-- Checks if all elements of the first list are contained in the second one
elems :: Eq a => [a] -> [a] -> Bool
elems [] l = True
elems (e:es) l = (e `elem` l) && elems es (l \\ [e])

-- Basically <$ for monads. <$ doesn't really work on the Parser monad
(>><) :: Monad m => m a -> b -> m b
m >>< b = m >> return b

