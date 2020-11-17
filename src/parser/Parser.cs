using System;
using System.Collections.Generic;
using System.Text;

namespace GoodBasic {
    namespace Parser {        
        interface Parser {
            (Token, string) Parse(string input);
            List<TokenType> Types();
        }
        
        class SelectFrom : Parser {
            private List<Parser> options;
            public SelectFrom(List<Parser> options) => this.options = options;
            
            public (Token, string) Parse(string input) {
                foreach(var parser in options) {
                    try {
                        var token = parser.Parse(input);
                        return token;
                    } catch(UnexpectedTypeException) {
                        continue;
                    }
                }
                
                throw new UnexpectedTypeException(Types());
            }
            
            public List<TokenType> Types() {
                var optTypes = new List<TokenType>();
                foreach(var parser in options) {
                    foreach(var type in parser.Types()) {
                        optTypes.Add(type);
                    }
                }
                return optTypes;
            }
        }
        
        class CreateFrom : Parser {
            private List<Parser> steps;
            private TokenType resultType;
            
            public CreateFrom(List<Parser> steps, TokenType resultType) {
                this.steps = steps;
                this.resultType = resultType;
            }
            
            public (Token, string) Parse(string input) {
                var currInp = input;
                var finalToken = new Token {
                    type = TokenType.None,
                    source = "",
                    children = new List<Token>()
                };
                foreach(var parser in steps) {
                    var result = parser.Parse(currInp);
                    if(finalToken.type == TokenType.None) {
                        finalToken = result.Item1;
                    } else {
                        finalToken += result.Item1;
                    }
                    currInp = result.Item2;
                }
                return (
                    new Token {
                        type = resultType,
                        source = finalToken.source,
                        children = finalToken.children
                    }, currInp
                );
            }
            
            public List<TokenType> Types() =>
                new List<TokenType> { resultType };
        }
        
        class Many : Parser {
            private Parser what;
            public Many(Parser what) => this.what = what;
            
            public (Token, string) Parse(string input) {
                // Parse first one out of loop so we fail on none
                var result = what.Parse(input);
                
                var finalToken = result.Item1;
                var currInpt = result.Item2;
                
                while(true) {
                    try {
                        result = what.Parse(currInpt);
                        finalToken += result.Item1;
                        currInpt = result.Item2;
                    } catch(UnexpectedTypeException) {
                        break;
                    }
                }
                
                return (finalToken, currInpt);
            }
            
            public List<TokenType> Types() => what.Types();
        }
        
        class AsType : Parser {
            private Parser what;
            private TokenType newType;
            
            public AsType(Parser what, TokenType newType) {
                this.what = what;
                this.newType = newType;
            }
            
            public (Token, string) Parse(string input) {
                var result = what.Parse(input);
                return (
                    new Token {
                        type = newType,
                        source = result.Item1.source,
                        children = result.Item1.children
                    }, result.Item2
                );
            }
            
            public List<TokenType> Types() => what.Types();
        }
        
        // Don't fail for optional stuff
        class Skip : Parser {
            public (Token, string) Parse(string input) =>
                (new Token {
                    type = TokenType.None,
                    source = "",
                    children = new List<Token>()
                }, input);
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.None };
        }
        
        class Char : Parser {
            private char c;
            public Char(char c) => this.c = c;
            
            public (Token, string) Parse(string input) {
                if(input.Length < 1 || input[0] != c) {
                    throw new UnexpectedTypeException(Types());
                } else {
                    return (
                        new Token {
                            type = TokenType.Character,
                            source = input.Substring(0, 1),
                            children = new List<Token>()
                        }, input.Substring(1)
                    );
                }
            }
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Character };
        }
        
        class Alpha : Parser {
            public (Token, string) Parse(string input) {
                if(input.Length < 1 || !char.IsLetter(input[0])) {
                    throw new UnexpectedTypeException(Types());
                } else {
                    return (
                        new Token {
                            type = TokenType.Character,
                            source = input.Substring(0, 1),
                            children = new List<Token>()
                        }, input.Substring(1)
                    );
                }
            }
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Character };
        }
        
        class Digit : Parser {
            public (Token, string) Parse(string input) {
                if(input.Length < 1 || !char.IsDigit(input[0])) {
                    throw new UnexpectedTypeException(Types());
                } else {
                    return (
                        new Token {
                            type = TokenType.Digit,
                            source = input.Substring(0, 1),
                            children = new List<Token>()
                        }, input.Substring(1)
                    );
                }
            }
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Digit };
        }
        
        class ParserException : Exception {
            public ParserException(string message) : base(message) {
            }
        }
        
        class UnexpectedTypeException : ParserException {
            public UnexpectedTypeException(List<TokenType> expectedTypes) :
                    base(typesToMessage(expectedTypes)) {
            }
            
            public static string typesToMessage(List<TokenType> types) {
                var msg = new StringBuilder(
                    "Parser Exception: Unexpected type! "
                    + "Expected Types: "
                );
                foreach(var type in types) {
                    msg.Append(Token.TypeStr(type)).Append(" ");
                }
                return msg.ToString();
            }
        }
    }
}