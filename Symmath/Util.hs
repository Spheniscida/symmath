module Symmath.Util where

import Data.List

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

isIntegral :: Double -> Bool
isIntegral x = (x - (fromIntegral . floor $ x)) == 0

elems :: Eq a => [a] -> [a] -> Bool
elems [e] l = e `elem` l
elems (e:es) l = case (e `elem` l) of
                    True -> elems es (l \\ [e])
                    False -> False

