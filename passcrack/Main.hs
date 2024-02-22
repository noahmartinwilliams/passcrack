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

passwords :: String -> [String]
passwords str = intern [(str !! 0)] str  where
    intern :: String -> String -> [String]
    intern prev chars = do
        let next = incrementChar prev chars
        prev : (intern next chars)

    incrementChar :: String -> String -> String
    incrementChar input chars | (P.length input) == 0 = [(chars !! 0)]
    incrementChar input chars = do
        let firstInputCharPos = getPos (input !! 0) chars 0
            (_ : rest) = input
        if firstInputCharPos == ((P.length chars) - 1) 
        then
            (chars !! 0) : (incrementChar rest chars)
        else
           (chars !! (firstInputCharPos + 1)) : rest

    getPos :: Char -> String -> Int -> Int
    getPos str [] _ = error ("Internal error: getPos called on charset that does not contain first character of input string: \"" P.++ [str] P.++ "\"")
    getPos c ( h : _ ) x | c == h = x
    getPos c ( _ : r ) x = getPos c r (x + 1)
        

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
    BS.append (BS.singleton (((hex2nibble f) `shiftL` 4) .|. (hex2nibble s) )) (hex2bits rest)

test :: BS.ByteString -> String -> (Bool, String)
test hex str = do
    (((hashlazy (fromString str)) == hex), str)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    args <- getArgs
    let target = hex2bits (args P.!! 0)
        pws = passwords "abcdefghijklmnopqrstuvwxyz"
        filtered = P.filter (\(x, _) -> x == True) (P.map (\x -> test target x) pws)
        (_, result) = filtered P.!! 0
    System.IO.putStr result
