using System;
using System.Collections.Generic;

namespace GoodBasic {
    namespace Parser {
        // <ident> ::= /[A-Za-z_][A-Za-z0-9_]+/
        class Ident : Parser {
            public (Token, string) Parse(string input) =>
                new SelectFrom(new List<Parser> {
                    new CreateFrom(
                        new List<Parser> {
                            new SelectFrom(new List<Parser> {
                                new Alpha(), new Char('_')
                            }), new Many(
                                new SelectFrom(new List<Parser> {
                                    new Alpha(), new Char('_'),
                                    new Digit()
                                })
                            )
                        }, TokenType.Ident
                    ), new AsType(
                        new SelectFrom(new List<Parser> {
                            new Alpha(), new Char('_')
                        }), TokenType.Ident
                    )
                }).Parse(input);
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Ident };
        }
        
        // <float> ::= /-?((.[0-9]+)|([0-9]+.)|([0-9]+.[0-9]+))(e[+-]?[0-9]+)/
        class Float : Parser {
            public (Token, string) Parse(string input) {
                var signParser = new SelectFrom(new List<Parser> {
                    new Char('+'), new Char('-'), new Skip()
                });
                var firstNumParser = new SelectFrom(new List<Parser> {
                    new CreateFrom(
                        new List<Parser> {
                            new Many(new Digit()), new Char('.'),
                            new Many(new Digit())
                        }, TokenType.Float
                    ), new CreateFrom(
                        new List<Parser> {
                            new Char('.'), new Many(new Digit())
                        }, TokenType.Float
                    ), new CreateFrom(
                        new List<Parser> {
                            new Many(new Digit()), new Char('.')
                        }, TokenType.Float
                    )
                });
                var exponParser = new SelectFrom(new List<Parser>{
                    new CreateFrom(
                        new List<Parser> { new Char('e'), new Integer() },
                        TokenType.Node
                    ), new Skip()
                });
                
                var sign = signParser.Parse(input);
                (Token, string) num;
                if(sign.Item1.type == TokenType.None) {
                    num = firstNumParser.Parse(input);
                } else {
                    num = firstNumParser.Parse(sign.Item2);
                    num.Item1 = sign.Item1 + num.Item1;
                }
                (Token, string) finalNum = exponParser.Parse(num.Item2);
                if(finalNum.Item1.type == TokenType.None) {
                    finalNum = num;
                } else {
                    finalNum = (num.Item1 + finalNum.Item1, finalNum.Item2);
                }
                finalNum.Item1.type = TokenType.Float;
                
                return finalNum;
            }
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Float };
        }
        
        // <int> ::= /-?[0-9]+/
        class Integer : Parser {
            public (Token, string) Parse(string input) =>
                new SelectFrom(new List<Parser> {
                    new CreateFrom(
                        new List<Parser> {
                            new SelectFrom(new List<Parser> {
                                new Char('+'), new Char('-')
                            }),
                            new Many(new Digit())
                        }, TokenType.Int
                    ), new AsType(new Many(new Digit()), TokenType.Int)
                }).Parse(input);
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Int };
        }
        
        // <string> ::= /'(\\.|[^\\'])*'/
        class Str : Parser {
            public (Token, string) Parse(string input) =>
                new SelectFrom(new List<Parser> {
                    new CreateFrom(
                        new List<Parser> {
                            new Char('\''),
                            new Many(
                                new SelectFrom(new List<Parser> {
                                    new CreateFrom(
                                        new List<Parser> {
                                            new Char('\\'),
                                            new AnyChar()
                                        }, TokenType.Character
                                    ), new AnyCharExcept("\\'")
                                })
                            ), new Char('\'')
                        }, TokenType.String
                    ), new CreateFrom(
                        new List<Parser> { new Char('\''), new Char('\'') },
                        TokenType.String
                    )
                }).Parse(input);
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.String };
        }
    }
}