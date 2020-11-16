module BasicParser  ( factor, memberAcc, funcCall, lambda, compOrRecDec
                    , ident, decimal, integer, string ) where

import Control.Applicative(Alternative(..))
import Parser
import Token

{-
 - <factor> ::= <ident> | <int> | <float> | <string>
 -            | lambda | <comp-rec-dec>
 -            | <member-acc> | <func-call> | '(' <expr> ')'
 -}
factor :: Parser Token
factor = do
    fac <- memberAcc <|> funcCall <|> lambda <|> compOrRecDec
        <|> ident <|> decimal <|> integer <|> string
        -- <|> step Factor [ char '(', expr, char ')' ]
    return $ CompToken Factor (source fac) [ fac ]

-- <member-acc> ::= <ident> ':' ( <ident> | <member-acc> )
memberAcc :: Parser Token
memberAcc = do
    name <- ident
    colon <- char ':'
    next <- memberAcc <|> ident
    return $ combineMany MemberAccess [ name, colon, next ]

-- <func-call> ::= 'call' <ident> { <expr> }
funcCall :: Parser Token
funcCall = do
    keyword <- chars "call"
    name <- ident
    --exprs <- multiple expr
    return $ combineMany FuncCall [ keyword, name{-, exprs-}]

{-
 - <lambda> ::= 'lambda' '(' [ <type-arg-list> ] ')' <type-name>
 -                  { <statement> /\n+/ }
 -              'end'
 -}
lambda :: Parser Token
lambda = do
    keyword <- chars "lambda"
    lpar <- char '('
    --args <- typeArgList
    rpar <- char ')'
    --tp <- typeName
    --stmts <- multiple statement
    endKey <- chars "end"
    return $ combineMany Lambda [ keyword, lpar, {-args,-} rpar{-, tp-}
                                ,{- stmts,-} endKey ]

-- <comp-rec-dec> ::= 'data' <ident> '(' [ <expr> { ',' <expr> } ] ')'
compOrRecDec :: Parser Token
compOrRecDec = do
    keyword <- chars "data"
    name <- ident
    lpar <- char '('
    -- [ <expr> { ',' <expr> } ]
    {-firstExpr <- expr
    nextExprs <- multiple $ step Expr [ char ',', expr ]
                <|> RawToken UndefToken ""
    let exprList =  if undefToken nextExprs then expr else
                        combine firstExpr nextExprs-}
    rpar <- char ')'
    return $
        combineMany CompOrRecDec [ keyword, name, lpar, {-exprList,-} rpar ]

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
