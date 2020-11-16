module Token(Token(..), TokenType(..), combine, undefToken) where

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
                    Character       | Digit         | UndefToken
                    deriving(Eq, Show)
data Token = RawToken   { tokenType :: TokenType
                        , source    :: String } |
            CompToken   { tokenType :: TokenType
                        , source    :: String
                        , children  :: [Token] }
            deriving(Eq, Show)
            
combine :: Token -> Token -> Token
combine (RawToken t1 s1) (RawToken t2 s2)
    | t1 /= t2 =
        CompToken UndefToken (s1 ++ s2) [ RawToken t1 s1, RawToken t2 s2 ]
    | otherwise = RawToken t1 (s1 ++ s2)
combine tok1 tok2 =
    CompToken UndefToken (source tok1 ++ source tok2) [ tok1, tok2 ]

undefToken :: Token -> Bool
undefToken tok = tokenType tok == UndefToken