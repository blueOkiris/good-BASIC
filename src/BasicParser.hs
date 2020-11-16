module BasicParser(ident, decimal, integer, string) where

import Control.Applicative(Alternative(..))
import Parser
import Token

-- <ident> ::= /[A-Za-z_][A-Za-z0-9_]+/
ident :: Parser Token
ident =
    step Ident  [ alpha <|> char '_'
                , multiple (alpha <|> char '_' <|> digit) ]
    <|> (do alpha <|> char '_')

-- <float> ::= /-?((.[0-9]+)|([0-9]+.)|([0-9]+.[0-9]+))(e[+-]?[0-9]+)/
decimal :: Parser Token
decimal = do
    sign <- char '-' <|> char '+' <|> return (RawToken UndefToken "")
    natNum <- step Decimal [ multiple digit, char '.', multiple digit ]
        <|> step Decimal [ multiple digit, char '.' ]
        <|> step Decimal [ char '.', multiple digit ]
    expon <- step UndefToken [ char 'e', integer ]
        <|> return (RawToken UndefToken "")
    let btmCombo = if undefToken expon then natNum else combine natNum expon
    let topCombo = if undefToken sign then btmCombo else combine sign btmCombo
    return topCombo { tokenType = Decimal }

-- <int> ::= /-?[0-9]+/
integer :: Parser Token
integer = do
        sign <- char '-' <|> char '+'
        natNum <- multiple digit
        return $ (combine sign natNum) { tokenType = IntegerType }
    <|> do
        natNum <- multiple digit
        return natNum { tokenType = IntegerType }

-- <string> ::= /'(\\.|[^\\'])*'/
string :: Parser Token
string = do
    startQuote <- char '\''
    midChars <-
        multiple    (   step Character [ char '\\', anyChar ]
                    <|> anyCharExcept [ '\'', '\\' ] )
        <|> return (RawToken UndefToken "")
    endQuote <- char '\''
    let btmCombo =  if undefToken midChars then
                        endQuote else combine midChars endQuote
    return $ (combine startQuote btmCombo) { tokenType = Str }
