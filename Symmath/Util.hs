module Symmath.Util where

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

isIntegral :: Double -> Bool
isIntegral x = if (x - (fromIntegral . floor $ x)) == 0 then True else False
