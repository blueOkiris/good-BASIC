module BasicParser(ident, float, integer, string) where

import Parser

-- <ident> ::= /[A-Za-z_][A-Za-z0-9_]+/
ident :: Parser
ident =
    selectFrom  [ doParsers [ fstChrOpts, multiple latrChrOpts] Ident
                , doParsers [ fstChrOpts ] Ident ]
    where
        fstChrOpts = selectFrom [ alpha, char '_' ]
        latrChrOpts = selectFrom [ alpha, char '_', digit ]

-- <float> ::= /-?((.[0-9]+)|([0-9]+.)|([0-9]+.[0-9]+))(e[+-]?[0-9]+)/
float :: Parser
float = 
    scienceNum
    where
        numSteps = selectFrom
            [ doParsers [ multiple digit, char '.', multiple digit ] Decimal
            , doParsers [ multiple digit, char '.' ] Decimal 
            , doParsers [ char '.', multiple digit ] Decimal ]
        signedNum = selectFrom
            [ doParsers [ selectFrom [ char '-', char '+' ], numSteps ] Decimal
            , numSteps ]
        scienceNum = selectFrom
            [ doParsers [ signedNum, char 'e', integer ] Decimal
            , signedNum ]
        

-- <int> ::= /-?[0-9]+/
integer :: Parser
integer = selectFrom
    [ doParsers [ selectFrom [ char '-', char '+' ], multiple digit] IntegerType
    , doParsers [ multiple digit ] IntegerType ]

-- <string> ::= /'(\\.|[^\\'])*'/
string :: Parser
string = doParsers
    [ char '\''
    , selectFrom    [ doParsers 
                        [ multiple $ selectFrom 
                            [ doParsers [ char '\\', anyChar ] UndefToken
                            , anyCharExcept [ '\'', '\\' ] ]
                        , char '\'' ] UndefToken
                    , char '\'' ] ] Str
