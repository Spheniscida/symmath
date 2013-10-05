module Symmath.Util where

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

fromMb :: Maybe a -> a
fromMb (Just a) = a

isIntegral :: Double -> Bool
isIntegral x = (x - (fromIntegral . floor $ x)) == 0
