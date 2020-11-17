module BasicParser where

import Control.Applicative(Alternative(..))
import Parser
import Token

{-
 - <statement> ::= <declaration> | <assignment> | <return> | <expr>
 -               | <statement> ';' <statement>
-}


{--- <expr> ::= { ( '!' | '~' ) } <product> { ( '++' | '--' ) }
expr :: Parser Token
expr =
    Expr `from` [ chars "!" <|> chars "~"
                , prod
                , chars "++" <|> chars "--" ]
    <|> Expr `from` [ chars "!" <|> chars "~", prod ]
    <|> Expr `from` [ prod, chars "++" <|> chars "--" ]
    <|> prod

-- <product> ::= <summation> { ( '*' | '/' | '%' ) <summation> }
prod :: Parser Token
prod =
    Product `from`  [ summation
                    , chars "*" <|> chars "/" <|> chars "%"
                    , summation ]
    <|> summation

-- <summation> ::= <shift> { ( '+' | '-' ) <shift> }
summation :: Parser Token
summation =
    Summation `from` [ shift, chars "+" <|> chars "-", shift ]
    <|> shift

-- <shift> ::= <inequality> { ( '<<' | '>>' ) <inequality> }
shift :: Parser Token
shift =
    Shift `from`    [ inequality
                    , chars "<<" <|> chars ">>"
                    , inequality ]
    <|> inequality

-- <inequality> ::= <equality> { ( '<' | '>' | '<=' | '>=' ) <equality> }
inequality :: Parser Token
inequality =
    Inequality `from`
        [ equality
        , chars "<=" <|> chars ">=" <|> chars "<" <|> chars ">"
        , equality ]
    <|> equality

-- <equality> ::= <mask-off> { ( '==' | '!=' ) <mask-off> }
equality :: Parser Token
equality =
    Equality `from` [ maskOff, chars "==" <|> chars "!=", maskOff ]
    <|> maskOff

-- <mask-off> ::= <exclusive> { '&' <exclusive> }
maskOff :: Parser Token
maskOff =
    MaskOff `from` [ exclusive, chars "&", exclusive ] <|> exclusive

-- <exclusive> ::= <mask-on> { '^' <mask-on> }
exclusive :: Parser Token
exclusive = Exclusive `from` [ maskOn, chars "^", maskOn ] <|> maskOn

-- <mask-on> ::= <conjunction> { '|' <conjunction> }
maskOn :: Parser Token
maskOn =
    MaskOn `from` [ conjunction, chars "&&", conjunction ]
    <|> conjunction

-- <conjunction> ::= <option> { '&&' <option> }
conjunction :: Parser Token
conjunction =
    Conjunction `from` [ option, chars "&&", option ] <|> option

-- <option> ::= <factor> { '||' <factor> }
option :: Parser Token
option = Option `from` [ factor, chars "||", factor ] <|> factor

{-
 - <factor> ::= <ident> | <int> | <float> | <string>
 -            | lambda | <comp-rec-dec>
 -            | <member-acc> | <func-call> | '(' <expr> ')'
 -}
factor :: Parser Token
factor = memberAcc <|> funcCall <|> lambda <|> compOrRecDec
        <|> ident <|> decimal <|> integer <|> string

-- <member-acc> ::= <ident> ':' ( <ident> | <member-acc> )
memberAcc :: Parser Token
memberAcc = do
    name <- ident
    colon <- chars ":"
    next <- memberAcc <|> ident
    return $ combineMany MemberAccess [ name, colon, next ]

-- <func-call> ::= 'call' <ident> { <expr> }
funcCall :: Parser Token
funcCall = do
    keyword <- chars "call"
    name <- ident
    exprs <- expr
    return $ combineMany FuncCall [ keyword, name, exprs ]

{-
 - <lambda> ::= 'lambda' '(' [ <type-arg-list> ] ')' <type-name>
 -                  { <statement> /\n+/ }
 -              'end'
 -}
lambda :: Parser Token
lambda = do
    keyword <- chars "lambda"
    lpar <- chars "("
    --args <- typeArgList
    rpar <- chars ")"
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
    lpar <- chars "("
    
    -- [ <expr> { ',' <expr> } ]
    firstExpr <- expr
    nextExprs <- multiple $ Expr `from` [ chars ",", expr ]
                <|> do return $ RawToken Node ""
    let exprList =  if undefToken nextExprs then firstExpr else
                        combine firstExpr nextExprs
    
    rpar <- chars ")"
    return $
        combineMany CompOrRecDec [ keyword, name, lpar, exprList, rpar ]-}

-- <ident> ::= /[A-Za-z_][A-Za-z0-9_]+/
ident :: Parser Token
ident = do
    name <- from    [ alpha <|> chars "_"
                    , multiple (alpha <|> chars "_" <|> digit) ]
            <|> alpha <|> chars "_"
    return $ name { tokenType = Ident }

-- <float> ::= /-?((.[0-9]+)|([0-9]+.)|([0-9]+.[0-9]+))(e[+-]?[0-9]+)/
decimal :: Parser Token
decimal = do
        sign <- getSign
        natNum <- getNatNum
        expon <- getExpon
        return (combine sign $ combine natNum expon) { tokenType = Decimal }
    <|> do
        sign <- getSign
        natNum <- getNatNum
        return (combine sign natNum) { tokenType = Decimal }
    <|> do
        natNum <- getNatNum
        expon <- getExpon
        return (combine natNum expon) { tokenType = Decimal }
    <|> do
        natNum <- getNatNum
        return natNum { tokenType = Decimal }
    where
        getSign = chars "-" <|> chars "+"
        getExpon = from [ chars "e", integer ]
        getNatNum =
            from [ multiple digit, char '.', multiple digit ]
            <|> from [ multiple digit, char '.' ]
            <|> from [ char '.', multiple digit ]

-- <int> ::= /-?[0-9]+/
integer :: Parser Token
integer = do
        sign <- chars "-" <|> chars "+"
        natNum <- multiple digit
        return $ (combine sign natNum) { tokenType = IntegerType }
    <|> do
        natNum <- multiple digit
        return natNum { tokenType = IntegerType }

-- <string> ::= /'(\\.|[^\\'])*'/
string :: Parser Token
string = 
    do str <- chars "''"; return str { tokenType = Str }
    <|> do
        startQuote <- char '\''
        midChars <- multiple (from [ chars "\\", anyChar ] <|> anyExcept "'\\")
        endQuote <- char '\''
        return (combine startQuote $ combine midChars endQuote)
            { tokenType = Str }
