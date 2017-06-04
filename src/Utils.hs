module Utils where

breakdown :: Integer -> [Integer] -> [Integer]
breakdown n (x:xs) = (n `div` x):breakdown (n `mod` x) xs
breakdown _ []     = []
