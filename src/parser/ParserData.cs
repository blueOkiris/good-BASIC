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
            Factor, CompOrRecDec, Lambda, FuncCall, MemberAcc
        }
        
        partial class Parser {
            private static Dictionary<Regex, TokenType> regexesToTokenTypes =
                    new Dictionary<Regex, TokenType>() {
                {
                    new Regex(
                        @"imports|exports|implements|def|fn|comp|rec|end|"
                            + @"lambda|data|int|float|str|mut|call"
                    ), TokenType.Keyword
                }, {
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
        }
        
        class CompoundToken : Token {
            private TokenType type;
            private List<Token> children;
            private int line;
            
            public CompoundToken(
                    TokenType type, List<Token> children, int line) {
                this.type = type;
                this.children =  new List<Token>();
                foreach(var child in children) {
                    this.children.Add(child);
                }
                this.line = line;
            }
            
            public TokenType Type() => type;
            public object Source() => children;
            public int Line() => line;
            
            private string tokStr(int indent) {
                var str = new StringBuilder();
                for(int i = 0; i < indent; i++) {
                    str.Append("|--");
                }
                str.Append("Compound { ");
                str.Append(type).Append(", ").Append(line).Append(", {\n");
                foreach(var child in children) {
                    if(child is RawToken) {
                        for(int i = 0; i < indent + 1; i++) {
                            str.Append("|--");
                        }
                        str.Append(child).Append("\n");
                    } else {
                        str.Append((child as CompoundToken).tokStr(indent + 1));
                        str.Append("\n");
                    }
                }
                for(int i = 0; i < indent; i++) {
                    str.Append("|--");
                }
                str.Append("}");
                return str.ToString();
            }
            
            public override string ToString() {
                return tokStr(0);
            }
        }
    }
}
