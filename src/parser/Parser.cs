using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;

namespace GoodBasic {
    namespace Parser {        
        interface Parser : IEnumerable {
            (Token, string) Parse(string input);
            List<TokenType> Types();
            IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
        }
        
        class Maybe : Parser {
            private Parser what;
            public Maybe(Parser what) => this.what = what;
            
            public (Token, string) Parse(string input) {
                try {
                    return what.Parse(input);
                } catch(UnexpectedTypeException) {
                    return (Token.FailureToken, input);
                }
            }
            
            public List<TokenType> Types() => what.Types();

            public IEnumerator<Parser> GetEnumerator() {
                yield return what;
            }
        }
        
        class SelectFrom : Parser {
            private List<Parser> options = new List<Parser>();
            
            public void Add(Parser option) => options.Add(option);

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

            public IEnumerator<Parser> GetEnumerator() {
                foreach(var parser in options) {
                    yield return parser;
                }
            }
        }
        
        class Create : Parser {
            private List<Parser> steps;
            private TokenType resultType;
            
            public Create(TokenType resultType) {
                this.steps = new List<Parser>();
                this.resultType = resultType;
            }
            
            public void Add(Parser step) => steps.Add(step);
            
            public (Token, string) Parse(string input) {
                var result = steps[0].Parse(input);
                var subSteps = new List<Parser>(steps.ToArray());
                subSteps.RemoveAt(0);
                
                var currInp = result.Item2;
                var finalToken = result.Item1;
                
                foreach(var parser in subSteps) {
                    result = parser.Parse(currInp);
                    
                    finalToken += result.Item1;
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

            public IEnumerator<Parser> GetEnumerator() {
                foreach(var parser in steps) {
                    yield return parser;
                }
            }
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

            public IEnumerator<Parser> GetEnumerator() {
                yield return what;
            }
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

            public IEnumerator<Parser> GetEnumerator() {
                yield return what;
            }
        }
        
        class AnyChar : Parser {
            public (Token, string) Parse(string input) {
                if(input.Length < 1) {
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

            public IEnumerator<Parser> GetEnumerator() {
                yield return new AnyChar();
            }
        }
        
        class AnyCharExcept : Parser {
            private string str;
            public AnyCharExcept(string str) => this.str = str;
            
            public (Token, string) Parse(string input) {
                if(input.Length < 1 || str.Contains(input[0])) {
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

            public IEnumerator<Parser> GetEnumerator() {
                yield return new AnyCharExcept(str);
            }
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

            public IEnumerator<Parser> GetEnumerator() {
                yield return new Char(c);
            }
        }
        
        class Word : Parser {
            private string str;
            public Word(string str) => this.str = str;
            
            public (Token, string) Parse(string input) {
                if(str.Length == 0) {
                    return (
                        new Token {
                            type = TokenType.Character,
                            source = "",
                            children = new List<Token>()
                        }, input
                    );
                }
                
                var result = new Char(str[0]).Parse(input);
                
                Token finalToken = result.Item1;
                string currInp = result.Item2;
                
                for(int i = 1; i < str.Length; i++) {
                    result = new Char(str[i]).Parse(currInp);
                    finalToken += result.Item1;
                    currInp = result.Item2;
                }
                
                return (finalToken, currInp);
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

            public IEnumerator<Parser> GetEnumerator() {
                yield return new Alpha();
            }
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

            public IEnumerator<Parser> GetEnumerator() {
                yield return new Digit();
            }
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