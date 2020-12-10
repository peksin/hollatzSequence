{-# LANGUAGE BangPatterns #-}

module Lib
    ( collatz,
      collatzIO,
      collatzSeq,
      collatzPair,
      generateIntList,
      calculateList,
      collatzToInt,
      printListAsString
    ) where

import Data.ByteString.Lazy.Char8 as BS

collatz :: Integer -> Integer
collatz n
    | even n    = n `div` 2
    | otherwise = 3 * n + 1

-- in case type IO Integer is required
collatzIO :: Integer -> IO Integer
collatzIO = pure

-- calculate the collatz sequence to a list
collatzSeq :: Integer -> [Integer]
collatzSeq 1 = []
collatzSeq n = collatz n : collatzSeq (collatz n)

-- get collatz sequence pair (number, steps)
collatzPair :: Integer -> (Integer, Integer)
collatzPair luku = (luku, fromIntegral . Prelude.length . collatzSeq $ luku)

generateIntList :: Integer -> [Integer]
generateIntList n = [1..n]

calculateList :: [Integer] -> [(Integer, Integer)]
calculateList = Prelude.map collatzPair

-- calculate collatzsequence up to an integer
collatzToInt :: Integer -> [(Integer, Integer)]
collatzToInt n = 
    let lista = generateIntList n in
        calculateList lista

-- print the calculated list as a String, with
-- each calculation pair on its own line
printListAsString :: [(Integer, Integer)] -> BS.ByteString
printListAsString = BS.concat . Prelude.map (\pari -> BS.pack (show pari) <> BS.pack "\n")