using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Text;

namespace GoodBasic {
    namespace Parser {            
        enum TokenType {
            // Raw tokens
            Identifier, FloatingPoint, Integer, String,
            Keyword, Symbol, NewLine,
            
            // Compound tokens
            Module, Import, Export, Implement, IdentList, MemAccList,
            Definition, FuncDef, TypeArgList,
            TypeName, TypeList, CompDef,RecDef,
            Statement, Declaration, Assignment, Return,
            Expr, Product, Summation, Shift, Inequality, Equality, MaskOff,
            Exclusive, MaskOn, Conjunction, Option,
            Factor, CompOrRecDec, Lambda, FuncCall, MemberAcc,
            Node
        }
        
        partial class Parser {
            private static Dictionary<Regex, TokenType> regexesToTokenTypes =
                    new Dictionary<Regex, TokenType>() {
                {
                    new Regex(@"[A-Za-z_][A-Za-z0-9_]+", RegexOptions.Compiled),
                    TokenType.Identifier
                }, {
                    new Regex(
                        @"[\+\-]?((\.[0-9]+)|([0-9]+\.)|([0-9]+\.[0-9]+))"
                            + @"(e[+\-]?[0-9]+)?",
                        RegexOptions.Compiled
                    ), TokenType.FloatingPoint
                }, {
                    new Regex(@"[\+\-]?[0-9]+", RegexOptions.Compiled),
                    TokenType.Integer
                }, {
                    new Regex(@"'(\\.\A|[^\\'])*'", RegexOptions.Compiled),
                    TokenType.String
                }
            };
        }
        
        interface Token {
            TokenType Type();
            object Source();
            int Line();
            
            Token Combined(Token other);
        }
        
        struct RawToken : Token {
            private TokenType type;
            private string source;
            private int line, col;
            
            public RawToken(
                    TokenType type, string source, int line, int col) {
                this.type = type;
                this.source = source;
                this.line = line;
                this.col = col;
            }
            
            public TokenType Type() => type;
            public object Source() => source;
            public int Line() => line;
            public int Col() => col;
            
            public override string ToString() {
                var tokStr = new StringBuilder("Raw { ");
                tokStr.Append(type).Append(", ");
                if(type == TokenType.NewLine) {
                    tokStr.Append("<br>, ");
                } else {
                    tokStr.Append("'").Append(source).Append("', ");
                }
                tokStr.Append(line).Append(":").Append(col).Append(" }");
                return tokStr.ToString();
            }
            
            public Token Combined(Token other) {
                if(other is RawToken && other.Type() == type) {
                    return new CompoundToken(
                        type, new List<Token> { this, other }, line
                    );
                } else if(other is RawToken) {
                    return new CompoundToken(
                        TokenType.Node, new List<Token> { this, other }, line
                    );
                } else {
                    var newChildren = new List<Token>();
                    newChildren.Add(this);
                    foreach(var child in other.Source() as List<Token>) {
                        newChildren.Add(child);
                    }
                    return new CompoundToken(other.Type(), newChildren, line);
                }
            }
        }
        
        class CompoundToken : Token {
            private TokenType type;
            private List<Token> children;
            private int line;
            
            public CompoundToken(
                    TokenType type, List<Token> children, int line) {
                this.type = type;
                this.children = children;
                this.line = line;
            }
            
            public TokenType Type() => type;
            public object Source() => children;
            public int Line() => line;
            
            public Token Combined(Token other) {
                if(other is CompoundToken && other.Type() == type) {
                    var newChildren = children;
                    foreach(var child in other.Source() as List<Token>) {
                        newChildren.Add(child);
                    }
                    return new CompoundToken(
                        type, newChildren, line
                    );
                } else if(other is CompoundToken) {
                    return new CompoundToken(
                        TokenType.Node, new List<Token> { this, other }, line
                    );
                } else {
                    var newChildren = new List<Token>();
                    foreach(var child in children) {
                        newChildren.Add(child);
                    }
                    newChildren.Add(other);
                    return new CompoundToken(
                        type, newChildren, line
                    );
                }
            }
        }
    }
}
