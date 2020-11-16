module Main(main) where

import System.IO(hSetBuffering, stdout, BufferMode(..))
import Parser(parse, digit, multiple)

testDigits :: IO()
testDigits = do
    putStr "Type value to parse digits from ('quit' to quit): "
    input <- getLine
    let digits = parse (multiple digit) input
    putStrLn $ show digits
    if input == "quit" then putStrLn "Done." else testDigits

main :: IO()
main = do
    hSetBuffering stdout NoBuffering
    testDigits
