module Parser   ( parse, digit, alpha, char, anyChar, anyCharExcept
                , multiple, selectFrom
                , Token(..), TokenType(..) ) where

import Data.Char(isDigit, isAlpha)

data TokenType =    Module          | Import        | Export        |
                    Implement       | IdentList     | Definition    |
                    FuncDef         | TypeArgList   | TypeName      |
                    CompDef         | RecDef        | Statement     |
                    Declaration     | Assignment    | Return        |
                    Expr            | Product       | Summation     |
                    Shift           | Inequality    | Equality      |
                    MaskOff         | Exclusive     | MaskOn        |
                    Conjunction     | Option        | Term          |
                    Factor          | MemberAccess  | FuncCall      |
                    Lambda          | CompOrRecDec  | Ident         |
                    Decimal         | IntegerType   | Str           |
                    Character       | Digit         | NoToken
                    deriving(Eq, Show)
data Token =    RawToke TokenType String | CompToke TokenType String [Token]
                deriving(Eq, Show)

tokenType :: Token -> TokenType
tokenType (RawToke tokType source) =
    tokType
tokenType (CompToke tokType source children) =
    tokType

tokenSource :: Token -> String
tokenSource (RawToke tokType source) =
    source
tokenSource (CompToke tokType source children) =
    source

tokenChildren :: Token -> [Token]
tokenChildren (RawToke tokType source) =
    []
tokenChildren (CompToke tokType source children) =
    children

type Parser = String -> ParseResult
type ParseResult = (Token, String)

parse :: Parser -> String -> ParseResult
parse parser input =
    parser input

digit :: String -> ParseResult
digit input
    | null input || not (isDigit $ head input) = (RawToke NoToken "", input)
    | otherwise = (RawToke Digit [ head input ], drop 1 input)

alpha :: String -> ParseResult
alpha input
    | null input || not (isAlpha $ head input) = (RawToke NoToken "", input)
    | otherwise = (RawToke Character [ head input ], drop 1 input)

anyChar :: String -> ParseResult
anyChar input
    | null input = (RawToke NoToken "", input)
    | otherwise = (RawToke Character [ head input ], drop 1 input)

char :: Char -> String -> ParseResult
char c input
    | null input || head input /= c = (RawToke NoToken "", input)
    | otherwise = (RawToke Character [ head input ], drop 1 input)

anyCharExcept :: [Char] -> String -> ParseResult
anyCharExcept chars input
    | null input || elem (head input) chars = (RawToke NoToken "", input)
    | otherwise = (RawToke Character [ head input ], drop 1 input)

multiple :: Parser -> (String -> ParseResult)
multiple parserFunc input =
    multipleCore "" firstType result parserFunc input
    where
        result = parse parserFunc input
        firstType = tokenType $ fst result

multipleCore ::
    String -> TokenType -> ParseResult -> Parser -> (String -> ParseResult)
multipleCore success tokType result parserFunc input
    | resultType /= NoToken =
        multipleCore (success ++ resultSrc) tokType newResult parserFunc input
    | otherwise = (RawToke tokType success, resultExtra)
    where
        resultType = tokenType $ fst result
        resultSrc = tokenSource $ fst result
        resultExtra = snd result
        newResult = parse parserFunc resultExtra

selectFrom :: [Parser] -> (String -> ParseResult)
selectFrom options input =
    selectFromCore 0 options input

selectFromCore :: Int -> [Parser] -> (String -> ParseResult)
selectFromCore index options input
    | index == length options = (RawToke NoToken "", input)
    | resultType /= NoToken = result
    | otherwise = selectFromCore (index + 1) options input
    where
        result = parse (options !! index) input
        resultType = tokenType $ fst result
