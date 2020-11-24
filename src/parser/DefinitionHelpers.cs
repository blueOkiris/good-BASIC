using System.Collections.Generic;

namespace GoodBasic {
    namespace Parser {
        partial class Parser {
            /*
             * <func-def> ::= 'def' 'fn' <ident> '(' [ <type-arg-list> ] ')' 
             *                        ( <type-name> | 'void' ) /\n+/
             *                    { <statement> /\n+/ } /\n+/
             *                '<end>'
             */
            private CompoundToken parseFuncDef() {
                if(lexInd >= lexemes.Length) {
                    throw new UnexpectedEOFException(-1);
                } else if((string) lexemes[lexInd].Source() != "def") {
                    throw new UnexpectedTokenException(
                        lexemes[lexInd].Type(),
                        new TokenType[] { TokenType.FuncDef },
                        lexemes[lexInd].Line()
                    );
                } else if(lexInd + 1 >= lexemes.Length) {
                    throw new UnexpectedEOFException(lexemes[lexInd].Line());
                } else if((string) lexemes[lexInd + 1].Source() != "fn") {
                    throw new UnexpectedTokenException(
                        lexemes[lexInd].Type(),
                        new TokenType[] { TokenType.FuncDef },
                        lexemes[lexInd].Line()
                    );
                } else if(lexInd + 2 >= lexemes.Length) {
                    throw new UnexpectedEOFException(lexemes[lexInd].Line());
                } else if(lexemes[lexInd + 2].Type() != TokenType.Identifier) {
                    throw new UnexpectedTokenException(
                        lexemes[lexInd].Type(),
                        new TokenType[] { TokenType.Identifier },
                        lexemes[lexInd].Line()
                    );
                } else if(lexInd + 3 >= lexemes.Length) {
                    throw new UnexpectedEOFException(lexemes[lexInd].Line());
                } else if((string) lexemes[lexInd + 3].Source() != "(") {
                    throw new UnexpectedTokenException(
                        lexemes[lexInd].Type(),
                        new TokenType[] { TokenType.Symbol },
                        lexemes[lexInd].Line()
                    );
                }
                var children = new List<Token>();
                children.Add(lexemes[lexInd++]);
                children.Add(lexemes[lexInd++]);
                children.Add(lexemes[lexInd++]);
                children.Add(lexemes[lexInd++]);
                
                var currInd = lexInd;
                try {
                    children.Add(parseTypeArgList());
                } catch(ParserException) {
                    lexInd = currInd;
                }
                
                if(lexInd >= lexemes.Length) {
                    throw new UnexpectedEOFException(children[0].Line());
                } else if((string) lexemes[lexInd].Source() != ")") {
                    throw new UnexpectedTokenException(
                        lexemes[lexInd].Type(),
                        new TokenType[] { TokenType.Symbol },
                        lexemes[lexInd].Line()
                    );
                }
                children.Add(lexemes[lexInd++]);
                
                try {
                    children.Add(parseTypeName());
                } catch(ParserException) {
                    if(lexInd >= lexemes.Length) {
                        throw new UnexpectedEOFException(children[0].Line());
                    } else if((string) lexemes[lexInd].Source() != "void") {
                        throw new UnexpectedTokenException(
                            lexemes[lexInd].Type(),
                            new TokenType[] { TokenType.TypeName },
                            lexemes[lexInd].Line()
                        );
                    }
                    children.Add(lexemes[lexInd++]);
                }
                if(lexInd >= lexemes.Length) {
                    throw new UnexpectedEOFException(children[0].Line());
                } else if(lexemes[lexInd].Type() != TokenType.NewLine) {
                    throw new UnexpectedTokenException(
                        lexemes[lexInd].Type(),
                        new TokenType[] { TokenType.NewLine },
                        lexemes[lexInd].Line()
                    );
                }
                children.Add(lexemes[lexInd++]);
                eatNewLines();
                
                while(lexInd < lexemes.Length
                        && (string) lexemes[lexInd].Source() != "end") {
                    children.Add(parseStatement());
                    if(lexInd >= lexemes.Length) {
                        throw new UnexpectedEOFException(children[0].Line());
                    } else if(lexemes[lexInd].Type() != TokenType.NewLine) {
                        throw new UnexpectedTokenException(
                            lexemes[lexInd].Type(),
                            new TokenType[] { TokenType.NewLine },
                            lexemes[lexInd].Line()
                        );
                    }
                    children.Add(lexemes[lexInd++]);
                    eatNewLines();
                }
                
                if(lexInd >= lexemes.Length) {
                    throw new UnexpectedEOFException(
                        children[children.Count - 1].Line()
                    );
                } else if((string) lexemes[lexInd].Source() != "end") {
                    throw new UnexpectedTokenException(
                        lexemes[lexInd].Type(),
                        new TokenType[] { TokenType.Keyword },
                        lexemes[lexInd].Line()
                    );
                }
                children.Add(lexemes[lexInd++]);
                
                return new CompoundToken(
                    TokenType.FuncDef, children, children[0].Line()
                );
            }
            
            private CompoundToken parseCompDef() {
                return new CompoundToken(
                    TokenType.CompDef,
                    new List<Token> {},
                    -1
                );
            }
            
            private CompoundToken parseRecDef() {
                return new CompoundToken(
                    TokenType.RecDef,
                    new List<Token> {},
                    -1
                );
            }
            
            /*
             * <type-arg-list> ::=
             *            <type-name> <ident> { ',' <type-name> <ident> }
             */
            private CompoundToken parseTypeArgList() {
                var list = new List<Token>();
                list.Add(parseTypeName());
                if(lexInd >= lexemes.Length) {
                    throw new UnexpectedEOFException(
                        list[0].Line()
                    );
                } else if(lexemes[lexInd].Type() != TokenType.Identifier) {
                    throw new UnexpectedTokenException(
                        lexemes[lexInd].Type(),
                        new TokenType[] { TokenType.Keyword },
                        lexemes[lexInd].Line()
                    );
                }
                list.Add(lexemes[lexInd++]);
                
                while(lexInd < lexemes.Length
                        && (string) lexemes[lexInd].Source() == ",") {
                    list.Add(lexemes[lexInd++]);
                    list.Add(parseTypeName());
                    if(lexInd >= lexemes.Length) {
                        throw new UnexpectedEOFException(
                            list[0].Line()
                        );
                    } else if(lexemes[lexInd].Type() != TokenType.Identifier) {
                        throw new UnexpectedTokenException(
                            lexemes[lexInd].Type(),
                            new TokenType[] { TokenType.Keyword },
                            lexemes[lexInd].Line()
                        );
                    }
                    list.Add(lexemes[lexInd++]);
                }
                
                return new CompoundToken(
                    TokenType.TypeArgList, list, list[0].Line()
                );
            }
            
            /*
             * <type-name> ::= 'int' | 'float' | 'str'
             *               | 'fn' '(' [ <type-list> ] ')' <type-name>
             *               | '(' <type-name> ')'
             *               | '[' <type-name> ']'
             *               | <ident>
             *               | 'mut' <type-name>
             */
            private CompoundToken parseTypeName() {
                if(lexInd >= lexemes.Length) {
                    throw new UnexpectedEOFException(-1);
                }
                
                if(lexemes[lexInd].Type() == TokenType.Keyword) {
                    switch((string) lexemes[lexInd].Source()) {
                        case "float":
                        case "str":
                        case "int":
                            return new CompoundToken(
                                TokenType.TypeName,
                                new List<Token> { lexemes[lexInd] },
                                lexemes[lexInd++].Line()
                            );
                        case "mut":
                            return new CompoundToken(
                                TokenType.TypeName,
                                new List<Token> {
                                    lexemes[lexInd++], parseTypeName()
                                }, lexemes[lexInd - 1].Line()
                            );
                        case "fn": {
                            var children = new List<Token>();
                            children.Add(lexemes[lexInd++]);
                            if(lexInd >= lexemes.Length) {
                                throw new UnexpectedEOFException(
                                    children[0].Line()
                                );
                            } else if(
                                    (string) lexemes[lexInd].Source() != "(") {
                                throw new UnexpectedTokenException(
                                    lexemes[lexInd].Type(),
                                    new TokenType[] { TokenType.Symbol },
                                    lexemes[lexInd].Line()
                                );
                            }
                            children.Add(lexemes[lexInd++]);
                            children.Add(parseTypeList());
                            if(lexInd >= lexemes.Length) {
                                throw new UnexpectedEOFException(
                                    children[0].Line()
                                );
                            } else if(
                                    (string) lexemes[lexInd].Source() != ")") {
                                throw new UnexpectedTokenException(
                                    lexemes[lexInd].Type(),
                                    new TokenType[] { TokenType.Symbol },
                                    lexemes[lexInd].Line()
                                );
                            }
                            children.Add(lexemes[lexInd++]);
                            children.Add(parseTypeName());
                            
                            return new CompoundToken(
                                TokenType.TypeName, children, children[0].Line()
                            );
                        }
                    }
                } else if((string) lexemes[lexInd].Source() != "("
                        && (string) lexemes[lexInd].Source() != "[") {
                    var children = new List<Token>();
                    children.Add(lexemes[lexInd++]);
                    children.Add(parseTypeName());
                    if(lexInd >= lexemes.Length) {
                        throw new UnexpectedEOFException(children[0].Line());
                    } else if(
                            ((string) children[0].Source() == "("
                                && (string) lexemes[lexInd].Source() != ")")
                            || ((string) children[0].Source() == "["
                                && (string) lexemes[lexInd].Source() != "]")) {
                        throw new UnexpectedTokenException(
                            lexemes[lexInd].Type(),
                            new TokenType[] { TokenType.Symbol },
                            lexemes[lexInd].Line()
                        );
                    }
                    return new CompoundToken(
                        TokenType.TypeName, children, children[0].Line()
                    );
                } else if(lexemes[lexInd].Type() == TokenType.Identifier) {
                    return new CompoundToken(
                        TokenType.TypeName, new List<Token> { lexemes[lexInd] },
                        lexemes[lexInd++].Line()
                    );
                }
                
                throw new UnexpectedTokenException(
                    lexemes[lexInd].Type(),
                    new TokenType[] {
                        TokenType.Identifier, TokenType.Symbol,
                        TokenType.Keyword
                    }, lexemes[lexInd].Line()
                );
            }
            
            private CompoundToken parseStatement() {
                return new CompoundToken(
                    TokenType.RecDef,
                    new List<Token> {},
                    -1
                );
            }
            
            // <type-list> ::= <type-name> { ',' <type-name> }
            private CompoundToken parseTypeList() {
                if(lexInd >= lexemes.Length) {
                    throw new UnexpectedEOFException(-1);
                } else if(lexemes[lexInd].Type() != TokenType.Identifier) {
                    throw new UnexpectedTokenException(
                        lexemes[lexInd].Type(),
                        new TokenType[] { TokenType.Identifier },
                        lexemes[lexInd].Line()
                    );
                }
                var children = new List<Token>();
                children.Add(lexemes[lexInd++]);
                
                while(lexInd < lexemes.Length
                        && (string) lexemes[lexInd].Source() == ",") {
                    children.Add(lexemes[lexInd++]);
                    if(lexInd >= lexemes.Length) {
                        throw new UnexpectedEOFException(children[0].Line());
                    } else if(lexemes[lexInd].Type() != TokenType.Identifier) {
                        throw new UnexpectedTokenException(
                            lexemes[lexInd].Type(),
                            new TokenType[] { TokenType.Identifier },
                            lexemes[lexInd].Line()
                        );
                    }
                    children.Add(lexemes[lexInd++]);
                }
                
                return new CompoundToken(
                    TokenType.TypeList, children, children[0].Line()
                );
            }
        }
    }
}
