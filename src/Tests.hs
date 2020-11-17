module Tests where

import Control.Applicative(Alternative(..))
import Parser
import BasicParser
import Token

testChars :: IO()
testChars = do
    putStr "Type value to parse characters from ('quit' to quit): "
    input <- getLine
    let parsed = parse (multiple anyChar) input
    print (parsed :: [(Token, String)])
    if input == "quit"  then putStrLn "Done." else testChars

testDigits :: IO()
testDigits = do
    putStr "Type value to parse digits from ('quit' to quit): "
    input <- getLine
    let parsed = parse (multiple digit) input
    print (parsed :: [(Token, String)])
    if input == "quit"  then putStrLn "Done." else testDigits

testAlternate :: IO()
testAlternate = do
    putStr "Type value to parse digits then alphas from ('quit' to quit): "
    input <- getLine
    let parsed = parse (from [ multiple digit, multiple alpha ]) input
    print (parsed :: [(Token, String)])
    if input == "quit"  then putStrLn "Done." else testAlternate

testBasics :: IO()
testBasics = do
    putStr "Type value to parse basic types from ('quit' to quit): "
    input <- getLine
    let parsed = parse (ident <|> decimal <|> integer <|> string) input
    print (parsed :: [(Token, String)])
    if input == "quit"  then putStrLn "Done." else testBasics
