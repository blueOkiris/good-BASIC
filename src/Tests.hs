module Tests where

import Parser
import BasicParser

testDigit :: IO()
testDigit = do
    putStr "Type value to parse a digit from ('quit' to quit): "
    input <- getLine
    let parsedDigit = parse digit input
    print parsedDigit
    if input == "quit" then putStrLn "Done." else testDigit

testDigits :: IO()
testDigits = do
    putStr "Type value to parse digits from ('quit' to quit): "
    input <- getLine
    let digits = parse (multiple digit) input
    print digits
    if input == "quit" then putStrLn "Done." else testDigits

testDecimal :: IO()
testDecimal = do
    putStr "Type value to parse a float from ('quit' to quit): "
    input <- getLine
    let number = parse decimal input
    print number
    if input == "quit" then putStrLn "Done." else testDecimal

testStrings :: IO()
testStrings = do
    putStr "Type value to parse a string from ('quit' to quit): "
    input <- getLine
    let str = parse string input
    print str
    if input == "quit" then putStrLn "Done." else testStrings

testExpr :: IO()
testExpr = do
    putStr "Type value to parse an expression from ('quit' to quit): "
    input <- getLine
    let str = parse expr input
    print str
    if input == "quit" then putStrLn "Done." else testExpr
