module Parser   ( Parser(..), Parsable(..)
                , failure, anyChar, multiple, from, anyExcept
                , char, alpha, digit, chars, wspace ) where
    
import Control.Monad(ap, liftM)
import Control.Applicative(Alternative(..))
import Data.Char(isAlpha, isDigit, isSpace)

newtype Parser a = Parser   { parse :: String -> [(a, String)] }

instance Monad Parser where
    -- do a b means a >>= b which is like a >> \s -> b
    -- Parser a -> (a -> Parser b) -> Parser b
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
    
class Parsable a where
    combine :: a -> a -> a
    fromChar :: Char -> a
    getSource :: a -> String
    
failure :: Parser a
failure = Parser $ const []

anyChar :: Parsable a => Parser a
anyChar = Parser $ \inp -> case inp of "" -> []; (c:cs) -> [(fromChar c,cs)]
    
condChar :: Parsable a => (Char -> Bool) -> Parser a
condChar check = do
    tok <- anyChar
    let src = getSource tok
    if null src || not (check $ head src) then
        failure
    else
        return tok
    
anyExcept :: Parsable a => [Char] -> Parser a
anyExcept str = condChar (`notElem` str)
    
char :: Parsable a => Char -> Parser a
char c = condChar (== c)

alpha :: Parsable a => Parser a
alpha = condChar isAlpha

digit :: Parsable a => Parser a
digit = condChar isDigit

wspace :: Parsable a => Parser a
wspace = condChar isSpace

chars :: Parsable a => [Char] -> Parser a
chars [] = failure
chars str
    | length str == 1 = char (head str)
    | otherwise = do
        first <- char $ head str
        next <- chars $ drop 1 str
        return $ combine first next

multiple :: Parsable a => Parser a -> Parser a
multiple parser =
    (do
        first <- parser
        next <- multiple parser
        return $ combine first next)
    <|> parser

from :: Parsable a => [Parser a] -> Parser a
from [] = failure
from steps
    | length steps == 1 = head steps
    | otherwise = do
        first <- head steps
        next <- from (drop 1 steps)
        return $ combine first next
