using System.Collections.Generic;

namespace GoodBasic {
    namespace Parser {
        /*
         * <factor> ::= <ident> | <int> | <float> | <string>
         *            | lambda | <comp-rec-dec>
         *            | <member-acc> | <func-call> | '(' <expr> ')'
         */
        class Factor : Parser {
            public (Token, string) Parse(string input) =>
                new SelectFrom {
                    new Lambda(), new CompOrRecDec(),
                    new MemberAcc(), new FuncCall(),
                    new Ident(), new Float(), new Integer(), new Str(),
                    new Create(TokenType.Expr) {
                        new Char('('), new Factor(), new Char(')')
                    }
                }.Parse(input);
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Factor };
        }        
        
        // <member-acc> ::= <ident> ':' ( <ident> | <member-acc> )
        class MemberAcc : Parser {
            public (Token, string) Parse(string input) {
                // Member access pieces
                var name = new Ident().Parse(input);
                var colon = new Char(':').Parse(name.Item2);
                var member = new SelectFrom {
                    new MemberAcc(), new Ident()
                }.Parse(colon.Item2);
                
                // Combine
                var memberAcc = name.Item1 + colon.Item1 + member.Item1;
                memberAcc.type = TokenType.MemberAccess;
                
                return (memberAcc, member.Item2);
            }
            
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.MemberAccess };
        }
        
        // <func-call> ::= 'call' <ident> { <expr> }
        class FuncCall : Parser {
            public (Token, string) Parse(string input) {
                // Function call items
                var callKeyword = new Word("call").Parse(input);
                    var sp1 = new SkipWhitespace().Parse(callKeyword.Item2);
                var name = new Ident().Parse(sp1.Item2);
                    var sp2 = new SkipWhitespace().Parse(name.Item2);
                var exprs = new Maybe(new Many(new Expr())).Parse(sp2.Item2);
                
                // Combine
                var funcCall = callKeyword.Item1 + name.Item1;
                if(exprs.Item1.type != TokenType.Failure) {
                    funcCall += exprs.Item1;
                }
                funcCall.type = TokenType.FuncCall;
                
                return (funcCall, exprs.Item2);
            }
            
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.FuncCall };
        }
        
        /*
         * <lambda> ::= 'lambda' '(' [ <type-arg-list> ] ')' <type-name> /\n+/
         *                  { <statement> /\n+/ }
         *              'end'
         */
        class Lambda : Parser {
            public (Token, string) Parse(string input) {
                // lambda items
                var lambdaKeyword = new Word("lambda").Parse(input);
                    var sp1 = new SkipWhitespace().Parse(lambdaKeyword.Item2);
                var lpar = new Char('(').Parse(sp1.Item2);
                    var sp2 = new SkipWhitespace().Parse(lpar.Item2);
                var arglist = new Maybe(new TypeArgList()).Parse(sp2.Item2);
                    var sp3 = new SkipWhitespace().Parse(arglist.Item2);
                var rpar = new Char(')').Parse(sp3.Item2);
                    var sp4 = new SkipWhitespace().Parse(rpar.Item2);
                var typeName = new TypeName().Parse(sp4.Item2);
                    var sp5 = new SkipWhitespace().Parse(typeName.Item2);
                var newLine = new Many(new Char('\n')).Parse(sp5.Item2);
                var statements = new Maybe(
                    new Create(TokenType.Node) {
                        new SkipWhitespace(), new Many(new Statement()),
                        new SkipWhitespace(), new Many(new Char('\n'))
                    }
                ).Parse(newLine.Item2);
                    var sp6 = new SkipWhitespace().Parse(statements.Item2);
                var endKeyword = new Word("end").Parse(sp6.Item2);
                
                // Combine
                var lambda = lambdaKeyword.Item1 + lpar.Item1;
                if(arglist.Item1.type != TokenType.Failure) {
                    lambda += arglist.Item1;
                }
                lambda += rpar.Item1;
                lambda += typeName.Item1;
                if(statements.Item1.type != TokenType.Failure) {
                    lambda += statements.Item1;
                }
                lambda += endKeyword.Item1;
                lambda.type = TokenType.Lambda;
                
                return (lambda, endKeyword.Item2);
            }
            
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Lambda };
        }
         
        // <comp-rec-dec>  ::= 'data' <ident> '(' [ <expr> { ',' <expr> } ] ')'
        class CompOrRecDec : Parser {
            public (Token, string) Parse(string input) {
                // Declaration items
                var dataKeyword = new Word("data").Parse(input);
                    var sp1 = new SkipWhitespace().Parse(dataKeyword.Item2);
                var name = new Ident().Parse(sp1.Item2);
                    var sp2 = new SkipWhitespace().Parse(name.Item2);
                var lpar = new Char('(').Parse(sp2.Item2);
                    var sp3 = new SkipWhitespace().Parse(lpar.Item2);
                var firstExpr = new Expr().Parse(sp3.Item2);
                var nextExprs = new Maybe(
                    new Many(
                        new Create(TokenType.Node) {
                            new SkipWhitespace(), new Char(','), new Expr()
                        }
                    )
                ).Parse(firstExpr.Item2);
                    var sp4 = new SkipWhitespace().Parse(nextExprs.Item2);
                var rpar = new Char(')').Parse(sp4.Item2);
                
                // Combine
                var declr =
                    dataKeyword.Item1 + name.Item1 + lpar.Item1
                    + firstExpr.Item1;
                if(nextExprs.Item1.type != TokenType.Failure) {
                    declr += nextExprs.Item1;
                }
                declr += rpar.Item1;
                declr.type = TokenType.CompOrRecDec;
                
                return (declr, rpar.Item2);
            }
            
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.CompOrRecDec };
        }
        
        // <ident> ::= /[A-Za-z_][A-Za-z0-9_]+/
        class Ident : Parser {
            public (Token, string) Parse(string input) {
                // Get ident pieces
                var firstChar = new SelectFrom {
                    new Alpha(), new Char('_')
                }.Parse(input);
                var followingChars = new Maybe(
                    new Many(
                        new SelectFrom {
                            new Alpha(), new Char('_'), new Digit()
                        }
                    )
                ).Parse(firstChar.Item2);
                
                // Combine
                var ident = firstChar.Item1;
                if(followingChars.Item1.type != TokenType.Failure) {
                    ident += followingChars.Item1;
                }
                ident.type = TokenType.Ident;
                
                // No keywords as identifiers
                if(ParserSettings.Keywords.Contains(ident.source)) {
                    throw new UnexpectedTypeException(Types());
                }
                
                return (ident, followingChars.Item2);
            }
                
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Ident };

            public IEnumerator<Parser> GetEnumerator() {
                yield return new Ident();
            }
        }
        
        // <float> ::= /-?((.[0-9]+)|([0-9]+.)|([0-9]+.[0-9]+))(e[+-]?[0-9]+)/
        class Float : Parser {
            public (Token, string) Parse(string input) {
                // Parse float items
                var sign = new Maybe(
                    new SelectFrom { new Char('+'), new Char('-') }
                ).Parse(input);
                var natNum = new SelectFrom{
                    new Create(TokenType.Float) {
                        new Many(new Digit()),
                        new Char('.'),
                        new Many(new Digit())
                    }, new Create(TokenType.Float) {
                        new Char('.'), new Many(new Digit())
                    }, new Create(TokenType.Float) {
                        new Many(new Digit()), new Char('.')
                    }
                }.Parse(sign.Item2);
                var expon = new Maybe(
                    new Create(TokenType.Node) { new Char('e'), new Integer() }
                ).Parse(natNum.Item2);
                
                // Combine
                var flt = natNum.Item1;
                if(sign.Item1.type != TokenType.Failure) {
                    flt = sign.Item1 + flt;
                }
                if(expon.Item1.type != TokenType.Failure) {
                    flt += expon.Item1;
                }
                flt.type = TokenType.Float;
                
                return (flt, expon.Item2);
            }
            
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Float };

            public IEnumerator<Parser> GetEnumerator() {
                yield return new Float();
            }
        }
        
        // <int> ::= /-?[0-9]+/
        class Integer : Parser {
            public (Token, string) Parse(string input) {
                // Parse integer items
                var sign = new Maybe(
                    new SelectFrom { new Char('+'), new Char('-') }
                ).Parse(input);
                var natNum = new Many(new Digit()).Parse(sign.Item2);
                
                // Combine them
                var integer = natNum.Item1;
                if(sign.Item1.type != TokenType.Failure) {
                    integer = sign.Item1 + integer;
                }
                integer.type = TokenType.Int;
                
                return (integer, natNum.Item2);
            }
                
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Int };

            public IEnumerator<Parser> GetEnumerator() {
                yield return new Integer();
            }
        }
        
        // <string> ::= /'(\\.|[^\\'])*'/
        class Str : Parser {
            public (Token, string) Parse(string input) {
                // Parse string items
                var startQuote = new Char('\'').Parse(input);
                var middleChars = new Maybe(
                    new Many(
                        new SelectFrom {
                            new Create(TokenType.Character) {
                                new Char('\\'), new AnyChar()
                            }, new AnyCharExcept("\\'")
                        }
                    )
                ).Parse(startQuote.Item2);
                var endQuote = new Char('\'').Parse(middleChars.Item2);
                
                // Combine them
                var str = startQuote.Item1;
                if(middleChars.Item1.type != TokenType.Failure) {
                    str += middleChars.Item1;
                }
                str += endQuote.Item1;
                str.type = TokenType.String;
                
                return (str, endQuote.Item2);
            }
                
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.String };

            public IEnumerator<Parser> GetEnumerator() {
                yield return new Str();
            }
        }
    }
}