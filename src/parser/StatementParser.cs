using System.Collections.Generic;

namespace GoodBasic {
    namespace Parser {
        /*
         * <statement> ::= <declaration> | <assignment> | <return> | <expr>
         *               | <statement> ';' <statement>
         */
        class Statement : Parser {
            public (Token, string) Parse(string input) {
                var stmt = new SelectFrom {
                    new Declaration(), new Assignment(), new Return(),
                    new Expr()
                }.Parse(input);
                var secondary = new Maybe(
                    new Create(TokenType.Node) {
                        new Char(';'), new Statement()
                    }
                ).Parse(stmt.Item2);
                
                var finalStmt = stmt.Item1;
                if(secondary.Item1.type != TokenType.Failure) {
                    finalStmt += secondary.Item1;
                    finalStmt.type = TokenType.Statement;
                }
                
                return (finalStmt, secondary.Item2);
            }
            
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Statement };
        }
        
        // <declaration> ::= 'let' <type-name> <ident> [ '=' <expr> ]
        class Declaration : Parser {
            public (Token, string) Parse(string input) {
                var letKeyword = new Word("let").Parse(input);
                var typeName = (Token.FailureToken, letKeyword.Item2);//new TypeName().Parse(letKeyword.Item2);
                var name = new Ident().Parse(typeName.Item2);
                var assignment = new Maybe(
                    new Create(TokenType.Assignment) {
                        new Char('='), new Expr()
                    }
                ).Parse(name.Item2);
                
                var decl = letKeyword.Item1 + typeName.Item1 + name.Item1;
                if(assignment.Item1.type != TokenType.Failure) {
                    decl += assignment.Item1;
                }
                decl.type = TokenType.Declaration;
                
                return (decl, assignment.Item2);
            }
            
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Declaration };
        }
        
        // <assignment> ::= <ident> '=' <expr>
        class Assignment : Parser {
            public (Token, string) Parse(string input) {
                var varName = new Ident().Parse(input);
                var equ = new Char('=').Parse(varName.Item2);
                var expr = new Expr().Parse(equ.Item2);
                
                var assignment = varName.Item1 + equ.Item1 + expr.Item1;
                return (assignment, expr.Item2);
            }
            
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Declaration };
        }
        
        // <return> ::= 'return' <expr>
        class Return : Parser {
            public (Token, string) Parse(string input) {
                var retKeyword = new Word("return").Parse(input);
                var expr = new Expr().Parse(retKeyword.Item2);
                
                var ret = retKeyword.Item1 + expr.Item1;
                return (ret, expr.Item2);
            }
            
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Declaration };
        }
    }
}