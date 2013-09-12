module Symmath.Util where

isIntegral :: Double -> Bool
isIntegral x = if (x - (fromIntegral . floor $ x)) == 0 then True else False
