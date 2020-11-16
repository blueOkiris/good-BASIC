module BasicParser  ( expr, prod, shift, inequality, equality, maskOff
                    , exclusive, maskOn, conjunction, option
                    , factor, memberAcc, funcCall, lambda, compOrRecDec
                    , ident, decimal, integer, string ) where

import Control.Applicative(Alternative(..))
import Parser
import Token

-- <expr> ::= { ( '!' | '~' ) } <product> { ( '++' | '--' ) }
expr :: Parser Token
expr =
    Expr `fromConcat` [ char '!' <|> char '~', prod, chars "++" <|> chars "--" ]
    <|> Expr `fromConcat` [ char '!' <|> char '~', prod ]
    <|> Expr `fromConcat` [ prod, chars "++" <|> chars "--" ]
    <|> prod

-- <product> ::= <summation> { ( '*' | '/' | '%' ) <summation> }
prod :: Parser Token
prod =
    Product `fromConcat`    [ summation
                            , char '*' <|> char '/' <|> char '%'
                            , summation ]
    <|> summation

-- <summation> ::= <shift> { ( '+' | '-' ) <shift> }
summation :: Parser Token
summation =
    Summation `fromConcat` [ shift, char '+' <|> char '-', shift ] <|> shift

-- <shift> ::= <inequality> { ( '<<' | '>>' ) <inequality> }
shift :: Parser Token
shift =
    Shift `fromConcat` [ inequality, chars "<<" <|> chars ">>", inequality ]
    <|> inequality

-- <inequality> ::= <equality> { ( '<' | '>' | '<=' | '>=' ) <equality> }
inequality :: Parser Token
inequality =
    Inequality `fromConcat`
        [ equality
        , chars "<=" <|> chars ">=" <|> char '<' <|> char '>'
        , equality ]
    <|> equality

-- <equality> ::= <mask-off> { ( '==' | '!=' ) <mask-off> }
equality :: Parser Token
equality =
    Equality `fromConcat` [ maskOff, chars "==" <|> chars "!=", maskOff ]
    <|> maskOff

-- <mask-off> ::= <exclusive> { '&' <exclusive> }
maskOff :: Parser Token
maskOff = MaskOff `fromConcat` [ exclusive, char '&', exclusive ] <|> exclusive

-- <exclusive> ::= <mask-on> { '^' <mask-on> }
exclusive :: Parser Token
exclusive = Exclusive `fromConcat` [ maskOn, char '^', maskOn ] <|> maskOn

-- <mask-on> ::= <conjunction> { '|' <conjunction> }
maskOn :: Parser Token
maskOn =
    MaskOn `fromConcat` [ conjunction, chars "&&", conjunction ] <|> conjunction

-- <conjunction> ::= <option> { '&&' <option> }
conjunction :: Parser Token
conjunction =
    Conjunction `fromConcat` [ option, chars "&&", option ] <|> option

-- <option> ::= <factor> { '||' <factor> }
option :: Parser Token
option = Option `fromConcat` [ factor, chars "||", factor ] <|> factor

{-
 - <factor> ::= <ident> | <int> | <float> | <string>
 -            | lambda | <comp-rec-dec>
 -            | <member-acc> | <func-call> | '(' <expr> ')'
 -}
factor :: Parser Token
factor = memberAcc <|> funcCall <|> lambda <|> compOrRecDec
        <|> ident <|> decimal <|> integer <|> string
        <|> Factor `fromConcat` [ char '(', expr, char ')' ]

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
    exprs <- multiple expr
    return $ combineMany FuncCall [ keyword, name, exprs ]

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
    firstExpr <- expr
    nextExprs <- multiple $ Expr `fromConcat` [ char ',', expr ]
                <|> do return $ RawToken UndefToken ""
    let exprList =  if undefToken nextExprs then firstExpr else
                        combine firstExpr nextExprs
    
    rpar <- char ')'
    return $
        combineMany CompOrRecDec [ keyword, name, lpar, exprList, rpar ]

-- <ident> ::= /[A-Za-z_][A-Za-z0-9_]+/
ident :: Parser Token
ident =
    Ident `fromConcat`
        [ alpha <|> char '_', multiple (alpha <|> char '_' <|> digit) ]
    <|> (do alpha <|> char '_')

-- <float> ::= /-?((.[0-9]+)|([0-9]+.)|([0-9]+.[0-9]+))(e[+-]?[0-9]+)/
decimal :: Parser Token
decimal = do
    sign <- char '-' <|> char '+' <|> return (RawToken UndefToken "")
    natNum <- Decimal `fromConcat` [ multiple digit, char '.', multiple digit ]
        <|> Decimal `fromConcat` [ multiple digit, char '.' ]
        <|> Decimal `fromConcat` [ char '.', multiple digit ]
    expon <- UndefToken `fromConcat` [ char 'e', integer ]
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
        multiple    (   Character `fromConcat` [ char '\\', anyChar ]
                    <|> anyCharExcept [ '\'', '\\' ] )
        <|> return (RawToken UndefToken "")
    endQuote <- char '\''
    let btmCombo =  if undefToken midChars then
                        endQuote else combine midChars endQuote
    return $ (combine startQuote btmCombo) { tokenType = Str }
