module Main (main) where

import Crypto.Hash.SHA256
import Data.List
import System.IO

passwords :: String -> [String]
passwords str = intern [(str !! 0)] str  where
    intern :: String -> String -> [String]
    intern prev chars = do
        let next = incrementChar prev chars
        prev : (intern next chars)

    incrementChar :: String -> String -> String
    incrementChar input chars = do
        if (length input) == 0
        then
            [(chars !! 0)]
        else do
            let firstInputCharPos = getPos (input !! 0) chars 0
                (_ : rest) = input
            if firstInputCharPos == ((length chars) - 1) 
            then
                (chars !! 0) : (incrementChar rest chars)
            else
               (chars !! (firstInputCharPos + 1)) : rest

    getPos :: Char -> String -> Int -> Int
    getPos str [] _ = error ("Internal error: getPos called on charset that does not contain first character of input string: \"" ++ [str] ++ "\"")
    getPos c ( h : _ ) x | c == h = x
    getPos c ( _ : r ) x = getPos c r (x + 1)
        

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    let pws = passwords "abcdefghijklmnopqrstuvwxyz"
        outputs = map (\x -> x ++ "\n") pws
        outStr = foldr (++) "" outputs
    putStr outStr
