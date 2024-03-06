{- 
This program is meant to demonstrate how to use the haskell parallel library to easily parallelize work done via lazily generated lists. 
It takes as an argument a sha256 checksum (as a hexadecimal string) and tries to find a password that would crack it (note: this does not deal with salts though).
If you don't know what that means then watch this video (How NOT to Store Passwords! by computerphile on youtube): https://www.youtube.com/watch?v=8ZtInClXe1Q 
-}

module Main (main) where

import Crypto.Hash.SHA256
import Data.List
import System.IO
import System.Environment
import Data.Bits
import Data.ByteString.Lazy.Internal as BSL
import Data.ByteString as BS
import Prelude as P
import Data.Word
import Data.String
import Control.Parallel.Strategies
import GHC.Conc
import Data.List.Split

passwordInts :: Int -> [[Int]] 
passwordInts clength = intern [0] (clength - 1) where

    intern :: [Int] -> Int -> [[Int]]
    intern prev clength = do
        let next = incrementChar prev clength
        prev : (intern (next `using` (rparWith rdeepseq)) clength )

    incrementChar :: [Int] -> Int -> [Int]
    incrementChar [] _ = [0]
    incrementChar ( h : t ) clength | h == clength = 0 : (incrementChar t clength)
    incrementChar ( h : t ) clength = ( h + 1 ) : t
        

hex2nibble :: Char -> Word8
hex2nibble '0' = 0
hex2nibble '1' = 1
hex2nibble '2' = 2
hex2nibble '3' = 3
hex2nibble '4' = 4
hex2nibble '5' = 5
hex2nibble '6' = 6
hex2nibble '7' = 7
hex2nibble '8' = 8
hex2nibble '9' = 9
hex2nibble 'A' = 10
hex2nibble 'B' = 11
hex2nibble 'C' = 12
hex2nibble 'D' = 13
hex2nibble 'E' = 14
hex2nibble 'F' = 15
hex2nibble 'a' = 10
hex2nibble 'b' = 11
hex2nibble 'c' = 12
hex2nibble 'd' = 13
hex2nibble 'e' = 14
hex2nibble 'f' = 15

hex2bits :: String -> BS.ByteString
hex2bits [] = empty
hex2bits ( f : s : rest ) = do
    let head = BS.singleton (((hex2nibble f) `shiftL` 4) .|. (hex2nibble s) )
    BS.append head (hex2bits rest)

test :: BS.ByteString -> String -> (Bool, String)
test hex str = do
    (((hash (fromString str)) == hex), str)

password :: [Int] -> String -> String
password [] _ = ""
password ( h : t ) chars = let first = ( chars !! h ) in (first `using` (rparWith rdeepseq)) : ( password t chars )

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    args <- getArgs
    let target = hex2bits (args P.!! 0)
        chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
        pws = passwordInts (P.length chars)
        answers = (P.map (\x -> test target (password x chars)) pws) 
        answers2 = (chunksOf 100 answers) `using` parBuffer numCapabilities rdeepseq
        filtered = P.filter (\(x, _) -> x == True) (P.foldr (++) [] answers2)
        (_, result) = filtered P.!! 0
    System.IO.putStr result
