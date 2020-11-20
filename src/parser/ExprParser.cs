using System.Collections.Generic;

namespace GoodBasic {
    namespace Parser {
        //<expr> ::= { ( '!' | '~' ) } <product> { ( '++' | '--' ) }
        class Expr : Parser {
            public (Token, string) Parse(string input) {
                var prefix = new Maybe(
                    new SelectFrom { new Char('!'), new Char('~') }
                ).Parse(input);
                var product = new Product().Parse(prefix.Item2);
                var suffix = new Maybe(
                    new Create(TokenType.Character) {
                        new SkipWhitespace(),
                        new SelectFrom { new Word("++"), new Word("--") }
                    }
                ).Parse(product.Item2);
                
                var expr = product.Item1;
                if(prefix.Item1.type != TokenType.Failure) {
                    expr = prefix.Item1 + expr;
                    expr.type = TokenType.Expr;
                }
                if(suffix.Item1.type != TokenType.Failure) {
                    expr += suffix.Item1;
                    expr.type = TokenType.Expr;
                }
                
                return (expr, suffix.Item2);
            }
            
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Expr };
        }
        
        //<product> ::= <summation> { ( '*' | '/' | '%' ) <summation> }
        class Product : Parser {
            public (Token, string) Parse(string input) {
                var prefix = new Summation().Parse(input);
                var suffix = new Maybe(
                    new Create(TokenType.Node) {
                        new SkipWhitespace(),
                        new SelectFrom {
                            new Char('*'), new Char('/'), new Char('%')
                        }, new SkipWhitespace(), new Summation()
                    }
                ).Parse(prefix.Item2);
                
                var token = prefix.Item1;
                if(suffix.Item1.type != TokenType.Failure) {
                    token += suffix.Item1;
                    token.type = TokenType.Product;
                }
                
                return (token, suffix.Item2);
            }
            
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Product };
        }
        
        //<summation> ::= <shift> { ( '+' | '-' ) <shift> }
        class Summation : Parser {
            public (Token, string) Parse(string input) {
                var prefix = new Shift().Parse(input);
                var suffix = new Maybe(
                    new Create(TokenType.Node) {
                        new SkipWhitespace(),
                        new SelectFrom { new Char('+'), new Char('-') },
                        new SkipWhitespace(), new Shift()
                    }
                ).Parse(prefix.Item2);
                
                var token = prefix.Item1;
                if(suffix.Item1.type != TokenType.Failure) {
                    token += suffix.Item1;
                    token.type = TokenType.Summation;
                }
                
                return (token, suffix.Item2);
            }
            
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Summation };
        }
        
        //<shift> ::= <inequality> { ( '<<' | '>>' ) <inequality> }
        class Shift : Parser {
            public (Token, string) Parse(string input) {
                var prefix = new Inequality().Parse(input);
                var suffix = new Maybe(
                    new Create(TokenType.Node) {
                        new SkipWhitespace(),
                        new SelectFrom { new Word(">>"), new Word("<<") },
                        new SkipWhitespace(), new Inequality()
                    }
                ).Parse(prefix.Item2);
                
                var token = prefix.Item1;
                if(suffix.Item1.type != TokenType.Failure) {
                    token += suffix.Item1;
                    token.type = TokenType.Shift;
                }
                
                return (token, suffix.Item2);
            }
            
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Shift };
        }
        
        //<inequality> ::= <equality> { ( '<' | '>' | '<=' | '>=' ) <equality> }
        class Inequality : Parser {
            public (Token, string) Parse(string input) {
                var prefix = new Equality().Parse(input);
                var suffix = new Maybe(
                    new Create(TokenType.Node) {
                        new SkipWhitespace(),
                        new SelectFrom {
                            new Char('<'), new Char('>'),
                            new Word("=="), new Word("!=")
                        }, new SkipWhitespace(), new Equality()
                    }
                ).Parse(prefix.Item2);
                
                var token = prefix.Item1;
                if(suffix.Item1.type != TokenType.Failure) {
                    token += suffix.Item1;
                    token.type = TokenType.Inequality;
                }
                
                return (token, suffix.Item2);
            }
            
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Inequality };
        }
        
        //<equality> ::= <mask-off> { ( '==' | '!=' ) <mask-off> }
        class Equality : Parser {
            public (Token, string) Parse(string input) {
                var prefix = new MaskOff().Parse(input);
                var suffix = new Maybe(
                    new Create(TokenType.Node) {
                        new SkipWhitespace(),
                        new SelectFrom { new Word("=="), new Word("!=") },
                        new SkipWhitespace(), new MaskOff()
                    }
                ).Parse(prefix.Item2);
                
                var token = prefix.Item1;
                if(suffix.Item1.type != TokenType.Failure) {
                    token += suffix.Item1;
                    token.type = TokenType.Equality;
                }
                
                return (token, suffix.Item2);
            }
            
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Equality };
        }
        
        //<mask-off> ::= <exclusive> { '&' <exclusive> }
        class MaskOff : Parser {
            public (Token, string) Parse(string input) {
                var prefix = new Exclusive().Parse(input);
                var suffix = new Maybe(
                    new Create(TokenType.Node) {
                        new SkipWhitespace(), new Char('&'),
                        new SkipWhitespace(), new Exclusive()
                    }
                ).Parse(prefix.Item2);
                
                var token = prefix.Item1;
                if(suffix.Item1.type != TokenType.Failure) {
                    token += suffix.Item1;
                    token.type = TokenType.MaskOff;
                }
                
                return (token, suffix.Item2);
            }
            
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.MaskOff };
        }
        
        //<exclusive> ::= <mask-on> { '^' <mask-on> }
        class Exclusive : Parser {
            public (Token, string) Parse(string input) {
                var prefix = new MaskOn().Parse(input);
                var suffix = new Maybe(
                    new Create(TokenType.Node) {
                        new SkipWhitespace(), new Char('^'),
                        new SkipWhitespace(), new MaskOn()
                    }
                ).Parse(prefix.Item2);
                
                var token = prefix.Item1;
                if(suffix.Item1.type != TokenType.Failure) {
                    token += suffix.Item1;
                    token.type = TokenType.Exclusive;
                }
                
                return (token, suffix.Item2);
            }
            
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Exclusive };
        }
        
        //<mask-on> ::= <conjunction> { '|' <conjunction> }
        class MaskOn : Parser {
            public (Token, string) Parse(string input) {
                var prefix = new Conjunction().Parse(input);
                var suffix = new Maybe(
                    new Create(TokenType.Node) {
                        new SkipWhitespace(), new Char('|'),
                        new SkipWhitespace(), new Conjunction()
                    }
                ).Parse(prefix.Item2);
                
                var token = prefix.Item1;
                if(suffix.Item1.type != TokenType.Failure) {
                    token += suffix.Item1;
                    token.type = TokenType.MaskOn;
                }
                
                return (token, suffix.Item2);
            }
            
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.MaskOn };
        }
        
        //<conjunction> ::= <option> { '&&' <option> }
        class Conjunction : Parser {
            public (Token, string) Parse(string input) {
                var prefix = new Option().Parse(input);
                var suffix = new Maybe(
                    new Create(TokenType.Node) {
                        new SkipWhitespace(), new Word("&&"),
                        new SkipWhitespace(), new Option()
                    }
                ).Parse(prefix.Item2);
                
                var token = prefix.Item1;
                if(suffix.Item1.type != TokenType.Failure) {
                    token += suffix.Item1;
                    token.type = TokenType.Conjunction;
                }
                
                return (token, suffix.Item2);
            }
            
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Conjunction };
        }
        
        //<option> ::= <factor> { '||' <factor> }
        class Option : Parser {
            public (Token, string) Parse(string input) {
                var fac = new Factor().Parse(input);
                var suffix = new Maybe(
                    new Create(TokenType.Node) {
                        new SkipWhitespace(), new Word("||"),
                        new SkipWhitespace(), new Factor()
                    }
                ).Parse(fac.Item2);
                
                var option = fac.Item1;
                if(suffix.Item1.type != TokenType.Failure) {
                    option += suffix.Item1;
                    option.type = TokenType.Option;
                }
                
                return (option, suffix.Item2);
            }
            
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Option };
        }
    }
}
