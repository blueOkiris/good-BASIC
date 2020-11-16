module Tests(testDigits, testStrings, testParseSelect, testDoParsers) where

import Parser
import BasicParser(string)

testDigits :: IO()
testDigits = do
    putStr "Type value to parse digits from ('quit' to quit): "
    input <- getLine
    let digits = parse (multiple digit) input
    print digits
    if input == "quit" then putStrLn "Done." else testDigits

testParseSelect :: IO()
testParseSelect = do
    putStr "Type value to parse digits or alphas from ('quit' to quit): "
    input <- getLine
    let vals = parse (selectFrom [ multiple digit, multiple alpha ]) input
    print vals
    if input == "quit" then putStrLn "Done." else testParseSelect

testDoParsers :: IO()
testDoParsers = do
    putStr "Type value to parse digit, alpha, digit from ('quit' to quit): "
    input <- getLine
    let vals = parse (doParsers [ digit, alpha, digit ] Str) input
    print vals
    if input == "quit" then putStrLn "Done." else testDoParsers

testStrings :: IO()
testStrings = do
    putStr "Type value to parse strings from ('quit' to quit): "
    input <- getLine
    let str = parse string input
    print str
    if input == "quit" then putStrLn "Done." else testStrings
