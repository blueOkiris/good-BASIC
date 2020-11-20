using System.Collections.Generic;

namespace GoodBasic {
    namespace Parser {                
        enum TokenType {
            // Raw tokens
            Identifier, FloatingPoint, Integer, String,
            Keyword, Character, NewLine,
            Failure,
            
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
        
        interface Token {
            TokenType Type();
            object Source();
            Token Combined(Token other);
        }
        
        struct RawToken : Token {
            private TokenType type;
            private string source;
            
            public RawToken(TokenType type, string source) {
                this.type = type;
                this.source = source;
            }
            
            public TokenType Type() => type;
            public object Source() => source;
            
            public Token Combined(Token other) {
                if(other is RawToken && other.Type() == type) {
                    return new CompoundToken(
                        type, new List<Token> { this, other }
                    );
                } else if(other is RawToken) {
                    return new CompoundToken(
                        TokenType.Node, new List<Token> { this, other }
                    );
                } else {
                    var newChildren = new List<Token>();
                    newChildren.Add(this);
                    foreach(var child in other.Source() as List<Token>) {
                        newChildren.Add(child);
                    }
                    return new CompoundToken(other.Type(), newChildren);
                }
            }
        }
        
        class CompoundToken : Token {
            private TokenType type;
            private List<Token> children;
            
            public CompoundToken(TokenType type, List<Token> children) {
                this.type = type;
                this.children = children;
            }
            
            public TokenType Type() => type;
            public object Source() => children;
            
            public Token Combined(Token other) {
                if(other is CompoundToken && other.Type() == type) {
                    var newChildren = children;
                    foreach(var child in other.Source() as List<Token>) {
                        newChildren.Add(child);
                    }
                    return new CompoundToken(
                        type, newChildren
                    );
                } else if(other is CompoundToken) {
                    return new CompoundToken(
                        TokenType.Node, new List<Token> { this, other }
                    );
                } else {
                    var newChildren = new List<Token>();
                    foreach(var child in children) {
                        newChildren.Add(child);
                    }
                    newChildren.Add(other);
                    return new CompoundToken(
                        type, newChildren
                    );
                }
            }
        }
    }
}
