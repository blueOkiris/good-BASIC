using System;
using System.Collections;
using System.Collections.Generic;

namespace GoodBasic {
    namespace Parser {
        // <ident> ::= /[A-Za-z_][A-Za-z0-9_]+/
        class Ident : Parser {
            public (Token, string) Parse(string input) =>
                new SelectFrom {
                    new Create(TokenType.Ident) {
                        new SelectFrom { new Alpha(), new Char('_') },
                        new Many(
                            new SelectFrom {
                                new Alpha(), new Char('_'), new Digit()
                            }
                        )
                    }, new AsType(
                        new SelectFrom { new Alpha(), new Char('_') },
                        TokenType.Ident
                    )
                }.Parse(input);
                
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Ident };

            public IEnumerator<Parser> GetEnumerator() {
                yield return new Ident();
            }
        }
        
        // <float> ::= /-?((.[0-9]+)|([0-9]+.)|([0-9]+.[0-9]+))(e[+-]?[0-9]+)/
        class Float : Parser {
            public (Token, string) Parse(string input) {
                var signParser = new SelectFrom {
                    new Char('+'), new Char('-'), new Skip()
                };
                var firstNumParser = new SelectFrom{
                    new Create(TokenType.Float) {
                        new Many(new Digit()),
                        new Char('.'),
                        new Many(new Digit())
                    }, new Create(TokenType.Float) {
                        new Char('.'), new Many(new Digit())
                    }, new Create(TokenType.Float) {
                        new Many(new Digit()), new Char('.')
                    }
                };
                var exponParser = new SelectFrom {
                    new Create(TokenType.Node) {
                        new Char('e'), new Integer()
                    }, new Skip()
                };
                
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

            public IEnumerator<Parser> GetEnumerator() {
                yield return new Float();
            }
        }
        
        // <int> ::= /-?[0-9]+/
        class Integer : Parser {
            public (Token, string) Parse(string input) =>
                new SelectFrom {
                    new Create(TokenType.Int) {
                        new SelectFrom { new Char('+'), new Char('-') },
                        new Many(new Digit())
                    }, new AsType(new Many(new Digit()), TokenType.Int)
                }.Parse(input);
                
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Int };

            public IEnumerator<Parser> GetEnumerator() {
                yield return new Integer();
            }
        }
        
        // <string> ::= /'(\\.|[^\\'])*'/
        class Str : Parser {
            public (Token, string) Parse(string input) =>
                new SelectFrom {
                    new Create(TokenType.String) {
                        new Char('\''),
                        new Many(
                            new SelectFrom {
                                new Create(TokenType.Character) {
                                    new Char('\\'),
                                    new AnyChar()
                                }, new AnyCharExcept("\\'")
                            }
                        ), new Char('\'')
                    }, new Create(TokenType.String) { 
                        new Char('\''), new Char('\'')
                    }
                }.Parse(input);
                
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.String };

            public IEnumerator<Parser> GetEnumerator() {
                yield return new Str();
            }
        }
    }
}