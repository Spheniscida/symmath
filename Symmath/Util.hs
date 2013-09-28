module Symmath.Util where

import Data.List

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

fromMb :: Maybe a -> a
fromMb (Just a) = a

isIntegral :: Double -> Bool
isIntegral x = if (x - (fromIntegral . floor $ x)) == 0 then True else False

elems :: Eq a => [a] -> [a] -> Bool
elems [e] l = e `elem` l
elems (e:es) l = case (e `elem` l) of
                    True -> elems es (l \\ [e])
                    False -> False
