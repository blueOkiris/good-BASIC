module Parser   ( Parser(..), failure, multiple, fromConcat
                , anyChar, char, chars, alpha, digit, anyCharExcept) where

import Control.Monad(ap, liftM)
import Control.Applicative(Alternative(..))
import Data.Char(isAlpha, isDigit)
import Token

newtype Parser a = Parser { parse :: String -> [(a, String)] }

instance Monad Parser where
    (>>=) psr1 tokToPsr2 =
        Parser $ \input ->
            let rslt1 = parse psr1 input in
                concatMap (\(tok, resInp) -> parse (tokToPsr2 tok) resInp) rslt1
    return token = Parser $ \input -> [(token, input)]
instance Applicative Parser where
    pure = return
    (<*>) = ap
instance Functor Parser where
    fmap = liftM
instance Alternative Parser where
    empty = failure
    (<|>) psr1 psr2 =
        Parser $ \input -> let res1 = parse psr1 input in
            case res1 of
                [] -> take 1 (parse psr2 input)
                (a:_) -> [ a ]
    
failure :: Parser a
failure = Parser $ const []

fromConcat :: TokenType -> [Parser Token] -> Parser Token
tokType `fromConcat` steps
    | null steps = failure
    | length steps == 1 = head steps
    | otherwise = do
        currStep <- head steps
        nextSteps <- tokType `fromConcat` (drop 1 steps)
        return $ (combine currStep nextSteps) { tokenType = tokType }

multiple :: Parser Token -> Parser Token
multiple parser = do
    first <- parser
    multiple <- multiple parser
    return $ combine first multiple
    <|> parser

anyChar :: Parser Token
anyChar =
    Parser $ \input ->
        [ (RawToken Character [ head input ], drop 1 input) | not $ null input ]

condChar :: (Char -> Bool) -> Parser Token
condChar check = do
    c <- anyChar
    if check (head $ source c) then return c else failure

char :: Char -> Parser Token
char c = condChar (== c)

chars :: [Char] -> Parser Token
chars str
    | null str = failure
    | length str == 1 = condChar (== head str)
    | otherwise = do
        curr <- condChar (== head str)
        next <- chars (drop 1 str)
        return $ combine curr next

alpha :: Parser Token
alpha = condChar isAlpha

digit :: Parser Token
digit = do
    character <- condChar isDigit
    return character { tokenType = Digit }

anyCharExcept :: [Char] -> Parser Token
anyCharExcept set = condChar (`notElem` set)
