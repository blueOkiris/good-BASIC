using System;
using System.Collections;
using System.Collections.Generic;

namespace GoodBasic {
    namespace Parser {
        //<expr> ::= { ( '!' | '~' ) } <product> { ( '++' | '--' ) }
        class Expr : Parser {
            public (Token, string) Parse(string input) =>
                new SelectFrom {
                    new Create(TokenType.Expr) {
                        new SelectFrom { new Char('!'), new Char('~') },
                        new Product(),
                        new SelectFrom { new Word("++"), new Word("--") }
                    }, new Create(TokenType.Expr) {
                        new Product(),
                        new SelectFrom { new Word("++"), new Word("--") }
                    }, new Create(TokenType.Expr) {
                        new SelectFrom { new Char('!'), new Char('~') },
                        new Product()
                    }, new Product()
                }.Parse(input);
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Expr };
        }
        
        //<product> ::= <summation> { ( '*' | '/' | '%' ) <summation> }
        class Product : Parser {
            public (Token, string) Parse(string input) =>
                new SelectFrom {
                    new Create(TokenType.Product) {
                        new Summation(), 
                        new SelectFrom {
                            new Char('*'), new Char('/'), new Char('%')
                        },
                        new Summation()
                    }, new Summation()
                }.Parse(input);
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Product };
        }
        
        //<summation> ::= <shift> { ( '+' | '-' ) <shift> }
        class Summation : Parser {
            public (Token, string) Parse(string input) =>
                new SelectFrom {
                    new Create(TokenType.Summation) {
                        new Shift(), 
                        new SelectFrom { new Char('+'), new Char('-') },
                        new Shift()
                    }, new Shift()
                }.Parse(input);
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Summation };
        }
        
        //<shift> ::= <inequality> { ( '<<' | '>>' ) <inequality> }
        class Shift : Parser {
            public (Token, string) Parse(string input) =>
                new SelectFrom {
                    new Create(TokenType.Shift) {
                        new Inequality(), 
                        new SelectFrom {
                            new Word("<<"), new Word(">>")
                        }, new Inequality()
                    }, new Inequality()
                }.Parse(input);
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Shift };
        }
        
        //<inequality> ::= <equality> { ( '<' | '>' | '<=' | '>=' ) <equality> }
        class Inequality : Parser {
            public (Token, string) Parse(string input) =>
                new SelectFrom {
                    new Create(TokenType.Inequality) {
                        new Equality(), 
                        new SelectFrom {
                            new Char('<'), new Char('>'),
                            new Word("<="), new Word(">=")
                        }, new Equality()
                    }, new Equality()
                }.Parse(input);
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Inequality };
        }
        
        //<equality> ::= <mask-off> { ( '==' | '!=' ) <mask-off> }
        class Equality : Parser {
            public (Token, string) Parse(string input) =>
                new SelectFrom {
                    new Create(TokenType.Equality) {
                        new MaskOff(), 
                        new SelectFrom {
                            new Word("=="), new Word("!=")
                        }, new MaskOff()
                    }, new MaskOff()
                }.Parse(input);
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Equality };
        }
        
        //<mask-off> ::= <exclusive> { '&' <exclusive> }
        class MaskOff : Parser {
            public (Token, string) Parse(string input) =>
                new SelectFrom {
                    new Create(TokenType.MaskOff) {
                        new Exclusive(), new Char('&'), new Exclusive()
                    }, new Exclusive()
                }.Parse(input);
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.MaskOff };
        }
        
        //<exclusive> ::= <mask-on> { '^' <mask-on> }
        class Exclusive : Parser {
            public (Token, string) Parse(string input) =>
                new SelectFrom {
                    new Create(TokenType.Exclusive) {
                        new MaskOn(), new Char('^'), new MaskOn()
                    }, new MaskOn()
                }.Parse(input);
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Exclusive };
        }
        
        //<mask-on> ::= <conjunction> { '|' <conjunction> }
        class MaskOn : Parser {
            public (Token, string) Parse(string input) =>
                new SelectFrom {
                    new Create(TokenType.MaskOn) {
                        new Conjunction(), new Char('|'), new Conjunction()
                    }, new Conjunction()
                }.Parse(input);
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.MaskOn };
        }
        
        //<conjunction> ::= <option> { '&&' <option> }
        class Conjunction : Parser {
            public (Token, string) Parse(string input) =>
                new SelectFrom {
                    new Create(TokenType.Conjunction) {
                        new Option(), new Word("&&"), new Option()
                    }, new Option()
                }.Parse(input);
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Conjunction };
        }
        
        //<option> ::= <factor> { '||' <factor> }
        class Option : Parser {
            public (Token, string) Parse(string input) =>
                new SelectFrom {
                    new Create(TokenType.Option) {
                        new Factor(), new Word("||"), new Factor()
                    }, new Factor()
                }.Parse(input);
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Option };
        }
    }
}
