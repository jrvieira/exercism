module Affine (decode, encode) where

import Data.List ( elemIndex )
import Data.List.Split ( chunksOf )
import Data.Char ( isAlphaNum, toLower )
import Control.Monad ( join )

m :: [Char]
m = ['a'..'z']

decode :: (Int, Int) -> String -> Maybe String
decode (a,b) s
   | 1 <- gcd a (length m) = Just $ map f $ join $ words s
   | otherwise = Nothing
   where
   f x
      | isAlphaNum x , Just y <- elemIndex x m = m !! mod (mmi a * (y - b)) (length m)
      | otherwise = x

-- modular multiplicative inverse
mmi :: Int -> Int
mmi a
   | i < 0 = i + length m
   | otherwise = i
   where
   (i,_,_) = egcd a (length m)

-- extended euclidean algorithm.
egcd :: Int -> Int -> (Int, Int, Int)
egcd a 0 = (1 , 0 , a)
egcd a b = (t , s-q*t , g)
   where
   (q,r) = quotRem a b
   (s,t,g) = egcd b r

encode :: (Int, Int) -> String -> Maybe String
encode (a,b) s
   | 1 <- gcd a (length m) = Just $ unwords $ chunksOf 5 $ f . toLower <$> filter isAlphaNum s
   | otherwise = Nothing
   where
   f x
      | Just i <- elemIndex x m = m !! mod (a * i + b) (length m)
      | otherwise = x


