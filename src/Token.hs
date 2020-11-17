module Token(Token(..), TokenType(..)) where
    
import Parser(Parsable(..))

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
                    Character       | Digit         | Node
                    deriving(Eq, Show)
data Token = RawToken   { tokenType :: TokenType
                        , source    :: String } |
            CompToken   { tokenType :: TokenType
                        , source    :: String
                        , children  :: [Token] }
            deriving(Eq, Show)

instance Parsable Token where
    pair (RawToken t1 s1) (RawToken t2 s2)
        | t1 /= t2 =
            CompToken Node (s1 ++ s2) [ RawToken t1 s1, RawToken t2 s2 ]
        | otherwise = RawToken t1 (s1 ++ s2)
    pair (CompToken Node s1 c1) (CompToken Node s2 c2) =
        CompToken Node (s1 ++ s2) (c1 ++ c2)
    pair tok1 (CompToken Node s2 c2) =
        CompToken Node (source tok1 ++ s2) ([ tok1 ] ++ c2)
    pair (CompToken Node s1 c1) tok2 =
        CompToken Node (s1 ++ source tok2) (c1 ++ [ tok2 ])
    pair tok1 tok2 =
        CompToken Node (source tok1 ++ source tok2) [ tok1, tok2 ]
    combine [] = RawToken Node ""
    combine toks
        | length toks == 1 = head toks
        | otherwise = pair (head toks) (combine $ drop 1 toks)
    fromChar c = RawToken Character [ c ]
    getSource tok = source tok
