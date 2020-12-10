{-# LANGUAGE BangPatterns #-}

module Lib
    ( collatz,
      collatzIO,
      collatzSeq,
      collatzPair,
      generateIntList,
      calculateList,
      collatzToInt,
      printListAsString,
      collatzMax,
      calculateIfNeeded
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

generateIntList :: Integer -> [(Integer, Integer)]
generateIntList n = lista where
    luvut = [1..collatzMax n]
    nollat = Prelude.replicate (fromIntegral . collatzMax $ n) 0
    lista = Prelude.zip luvut nollat

-- map works just fine when no optimization algorithm is used
--calculateList = Prelude.map collatzPair
calculateList :: [(Integer, Integer)] 
              -> Integer 
              -> Integer
              -> [(Integer, Integer)]
calculateList lista 0 max = tulos where
    valitulos = (1, 0)
    tulosArraySeuraavaan = valitulos : Prelude.tail lista 
    tulos = calculateList tulosArraySeuraavaan 1 max
calculateList lista curr max = tulos where
    valitulos = calculateIfNeeded lista (lista !! fromInteger curr)
    tulosArraySeuraavaan =
        (Prelude.take (fromInteger curr) lista ++ 
        [valitulos]) ++ Prelude.drop (fromInteger curr + 1) lista
    tulos = if max - curr >= 1 then calculateList tulosArraySeuraavaan (fromInteger curr + 1) max
    else tulosArraySeuraavaan
    


-- this function gets fed with the answer pairs already
-- calculated, checks if the new integer to be calculated
-- already exists in the answers and either gives the answer
-- that has already been calculated or calculates the next step
calculateIfNeeded :: [(Integer, Integer)] 
                 -> (Integer, Integer)
                 -> (Integer, Integer)
calculateIfNeeded vastaukset laskettavaLuku = go laskettavaLuku where
    go laskettavaLuku = 
        if snd laskettavaLuku == 0 && snd laskettavaLuku /= 1
        then if even (fst laskettavaLuku)
             then go (vastaukset !! fromIntegral ((fst laskettavaLuku `div` 2) - 1))
             else go (vastaukset !! fromIntegral (fst laskettavaLuku * 3 + 1 - 1))
        else laskettavaLuku
              


   -- if snd laskettavaLuku == 0 
   --     then collatzPair (fst laskettavaLuku)
   --     else laskettavaLuku


-- calculate collatzsequence up to an integer
-- this is what creates the ultimate list so
-- so the best optimizations are probably
-- made here!
collatzToInt :: Integer -> [(Integer, Integer)]
collatzToInt n = 
    let lista = generateIntList n in
        calculateList lista 0 (n - 1)

collatzMax :: Integer -> Integer
collatzMax = Prelude.maximum . collatzSeq

-- print the calculated list as a String, with
-- each calculation pair on its own line
printListAsString :: [(Integer, Integer)] -> BS.ByteString
printListAsString = BS.concat . Prelude.map (\pari -> BS.pack (show pari) <> BS.pack "\n")