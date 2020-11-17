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

testExpr :: IO()
testExpr = do
    putStr "Type value to parse expressions from ('quit' to quit): "
    input <- getLine
    let parsed = parse expr input
    print (parsed :: [(Token, String)])
    if input == "quit"  then putStrLn "Done." else testExpr

weakExpr :: Parser Token
weakExpr = do
        e <- from [ weakTerm, char '*' <|> char '/', weakTerm ]
        return e
    <|> weakTerm

weakTerm :: Parser Token
weakTerm = do
        t <- from [ weakFactor, char '+' <|> char '-', weakFactor ]
        return t
    <|> weakFactor

weakFactor :: Parser Token
weakFactor =
    weakFuncCall
    <|> ident <|> decimal <|> integer <|> string
    <|> do
        fac <- from [ char '(', weakExpr, char ')' ]
        return fac { tokenType = Factor }
    
weakFuncCall :: Parser Token
weakFuncCall = do
    keyword <- chars "call"
    s <- multiple wspace
    name <- ident
    s2 <- multiple wspace
    e <- expr
    return $ combine keyword $ combine s $ combine name $ combine s2 e    

testWeakExpr :: IO()
testWeakExpr = do
    putStr "Type value to parse weak expressions from ('quit' to quit): "
    input <- getLine
    let parsed = parse weakExpr input
    print (parsed :: [(Token, String)])
    if input == "quit"  then putStrLn "Done." else testWeakExpr
