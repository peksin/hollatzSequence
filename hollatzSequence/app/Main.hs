module Main where

import Lib
import Data.ByteString.Lazy.Char8 as BS

-- File IO is the bottleneck. Calculation goes by in less than
-- a second.

main :: IO ()
main = do
    n <- getNumber
    let file = "calc.txt"
    let laskut = collatzToInt n
    Prelude.putStrLn "laskut valmiit!"
    -- print . Prelude.last $ laskut
    BS.writeFile file (printListAsString laskut)
    Prelude.putStrLn "all done!"


getNumber :: IO Integer
getNumber = do
    Prelude.putStrLn "To what integer would you like to make the calculation?"
    number <- getLine
    let n = read number :: Integer
    return n
