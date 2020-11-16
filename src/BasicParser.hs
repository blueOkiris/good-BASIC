module BasicParser(string) where

import Debug.Trace(trace)
import Parser

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
