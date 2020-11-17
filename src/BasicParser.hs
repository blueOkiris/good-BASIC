module BasicParser where

import Control.Applicative(Alternative(..))
import Parser
import Token

{-
 - <statement> ::= <declaration> | <assignment> | <return> | <expr>
 -               | <statement> ';' <statement>
-}


-- <expr> ::= { ( '!' | '~' ) } <product> { ( '++' | '--' ) }
expr :: Parser Token
expr = do
        e <- from [ char '!' <|> char '~', prod, chars "++" <|> chars "--" ]
        return e { tokenType = Expr }
    <|> do
        e <- from [ char '!' <|> char '~', prod ]
        return e { tokenType = Expr }
    <|> do
        e <- from [ prod, chars "++" <|> chars "--" ]
        return e { tokenType = Expr }
    <|> do
        e <- from [ char '(', prod, char ')' ]
        return e { tokenType = Expr }
    <|> prod

-- <product> ::= <summation> { ( '*' | '/' | '%' ) <summation> }
prod :: Parser Token
prod = do
        p <- from [ summation, char '*' <|> char '/' <|> char '%', summation ]
        return p { tokenType = Product }
    <|> summation

-- <summation> ::= <shift> { ( '+' | '-' ) <shift> }
summation :: Parser Token
summation = do
        s <- from [ shift, char '+' <|> char '-', shift ]
        return s { tokenType = Summation }
    <|> shift

-- <shift> ::= <inequality> { ( '<<' | '>>' ) <inequality> }
shift :: Parser Token
shift = do
        s <- from [ inequality, chars "<<" <|> chars ">>", inequality ]
        return s { tokenType = Shift }
    <|> inequality

-- <inequality> ::= <equality> { ( '<' | '>' | '<=' | '>=' ) <equality> }
inequality :: Parser Token
inequality = do
        i <- from   [ equality
                    , chars "<=" <|> chars ">=" <|> char '<' <|> char '>'
                    , equality ]
        return i { tokenType = Inequality }
    <|> equality

-- <equality> ::= <mask-off> { ( '==' | '!=' ) <mask-off> }
equality :: Parser Token
equality = do
        e <- from [ maskOff, chars "==" <|> chars "!=", maskOff ]
        return e { tokenType = Equality }
    <|> maskOff

-- <mask-off> ::= <exclusive> { '&' <exclusive> }
maskOff :: Parser Token
maskOff = do
        m <- from [ exclusive, char '&', exclusive ]
        return m { tokenType = MaskOff }
    <|> exclusive

-- <exclusive> ::= <mask-on> { '^' <mask-on> }
exclusive :: Parser Token
exclusive = do
        e <- from [ maskOn, char '^', maskOn ]
        return e { tokenType = Exclusive }
    <|> maskOn

-- <mask-on> ::= <conjunction> { '|' <conjunction> }
maskOn :: Parser Token
maskOn = do
        m <- from [ conjunction, char '|', conjunction ]
        return m { tokenType = MaskOn }
    <|> conjunction

-- <conjunction> ::= <option> { '&&' <option> }
conjunction :: Parser Token
conjunction = do
        c <- from [ option, chars "&&", option ] 
        return c { tokenType = Conjunction }
    <|> option

-- <option> ::= <factor> { '||' <factor> }
option :: Parser Token
option = do
        o <- from [ factor, chars "||", factor ]
        return o { tokenType = Option }
    <|> factor

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
    colon <- char ':'
    next <- memberAcc <|> ident
    return (combine name $ combine colon next) { tokenType = MemberAccess }

-- <func-call> ::= 'call' <ident> { <expr> }
funcCall :: Parser Token
funcCall = do
    keyword <- chars "call"
    sp1 <- multiple wspace
    name <- ident
    sp2 <- multiple wspace <|> do return $ RawToken Character " "
    exprs <- multiple expr
    return
        (combine keyword $ combine sp1 $ combine name $ combine sp2 exprs)
            { tokenType = FuncCall }

{-
 - <lambda> ::= 'lambda' '(' [ <type-arg-list> ] ')' <type-name>
 -                  { <statement> /\n+/ }
 -              'end'
 -}
lambda :: Parser Token
lambda = do
    keyword <- chars "lambda"
    sp1 <- multiple wspace <|> do return $ RawToken Character " "
    lpar <- char '('
    sp2 <- multiple wspace <|> do return $ RawToken Character " "
    --args <- typeArgList
    rpar <- char ')'
    sp3 <- multiple wspace <|> do return $ RawToken Character " "
    --tp <- typeName
    --stmts <- multiple statement
    endKey <- chars "end"
    return 
        (combine keyword $ combine sp1 $
            combine lpar $ combine sp2 {-$ combine args-} $ combine rpar $
                combine sp3 {-$ combine tp $ combine stmts-} endKey)
                    { tokenType = Lambda }

-- <comp-rec-dec> ::= 'data' <ident> '(' [ <expr> { ',' <expr> } ] ')'
compOrRecDec :: Parser Token
compOrRecDec = do
    keyword <- chars "data"
    sp1 <- multiple wspace
    name <- ident
    sp2 <- multiple wspace <|> do return $ RawToken Character " "
    lpar <- char '('
    exprList <- multiple expr
    rpar <- char ')'
    return
        (combine keyword $ combine sp1 $ combine name $ combine sp2 $
            combine lpar $ combine exprList rpar)
                { tokenType = CompOrRecDec }

-- <ident> ::= /[A-Za-z_][A-Za-z0-9_]+/
ident :: Parser Token
ident = do
    name <- from    [ alpha <|> char '_'
                    , multiple (alpha <|> char '_' <|> digit) ]
            <|> alpha <|> char '_'
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
