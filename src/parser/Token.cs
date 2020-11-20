using System;
using System.Text;
using System.Collections.Generic;

namespace GoodBasic {
    namespace Parser {
        enum TokenType {
            Module, Import, Export, Implement, IdentList, Definition,
            FuncDef, TypeArgList, TypeName, CompDef, RecDef, Statement,
            Declaration, Assignment, Return, Expr, Product, Summation,
            Shift, Inequality, Equality, MaskOff, Exclusive, MaskOn,
            Conjunction, Option, Factor, MemberAccess, FuncCall, Lambda,
            CompOrRecDec, Ident, Float, Int, String, Character, Digit,
            TypeList,
            Node, Failure
        }
        
        struct Token {
            public TokenType type;
            public string source;
            public List<Token> children;
            
            public static Token FailureToken = new Token {
                type = TokenType.Failure,
                source = "",
                children = new List<Token>()
            };
            
            public override string ToString() {
                var tokStr = new StringBuilder();
                tokStr.Append("{ ").Append(TypeStr(type)).Append(", ");
                tokStr.Append(source).Append(", { ");
                foreach(var child in children) {
                    tokStr.Append(child.ToString()).Append(" ");
                }
                tokStr.Append("} }");
                return tokStr.ToString();
            }
            
            public static List<Token> operator |(Token a, Token b) {
                var comboChildren = new List<Token>();
                foreach(var child in a.children) {
                    comboChildren.Add(child);
                }
                foreach(var child in b.children) {
                    comboChildren.Add(child);
                }
                return comboChildren;
            }
            
            public static Token operator +(Token a, Token b) {
                Token result;
                if(a.type == b.type || a.type == TokenType.Node) {
                    result = new Token {
                        type = a.type,
                        source = a.source + ' ' + b.source,
                        children = a | b
                    };
                } else if(b.type == TokenType.Node) {
                    result = new Token {
                        type = b.type,
                        source = a.source + ' ' + b.source,
                        children = a | b
                    };
                } else {
                    result = new Token {
                        type = TokenType.Node,
                        source = a.source + ' ' + b.source,
                        children = new List<Token> { a, b }
                    };
                }
                // Uncomment for debug help
                /*Console.WriteLine(
                    "Combining {0} with {1} yields {2}", a, b, result
                );*/
                return result;
            }
            
            public static string TypeStr(TokenType type) {
                switch(type) {
                    case TokenType.Module: return "module";
                    case TokenType.Import: return "import";
                    case TokenType.Export: return "export"; 
                    case TokenType.Implement: return "implement";
                    case TokenType.IdentList: return "ident-list";
                    case TokenType.Definition: return "def";
                    case TokenType.FuncDef: return "func-def";
                    case TokenType.TypeArgList: return "type-arg-list";
                    case TokenType.TypeName: return "type-name";
                    case TokenType.CompDef: return "comp-def";
                    case TokenType.RecDef: return "rec-def";
                    case TokenType.Statement: return "stmt";
                    case TokenType.Declaration: return "decl";
                    case TokenType.Assignment: return "assign";
                    case TokenType.Return: return "ret";
                    case TokenType.Expr: return "expr";
                    case TokenType.Product: return "prod";
                    case TokenType.Summation: return "sum";
                    case TokenType.Shift: return "shift";
                    case TokenType.Inequality: return "ineq";
                    case TokenType.Equality: return "eq";
                    case TokenType.MaskOff: return "mask-off";
                    case TokenType.Exclusive: return "exclusive";
                    case TokenType.MaskOn: return "mask-on";
                    case TokenType.Conjunction: return "conj";
                    case TokenType.Option: return "opt";
                    case TokenType.Factor: return "factor";
                    case TokenType.MemberAccess: return "mem-acc";
                    case TokenType.FuncCall: return "func-call";
                    case TokenType.Lambda: return "lambda";
                    case TokenType.CompOrRecDec: return "comp-rec-dec";
                    case TokenType.Ident: return "ident";
                    case TokenType.Float: return "float";
                    case TokenType.Int: return "int"; 
                    case TokenType.String: return "str"; 
                    case TokenType.Character: return "char";
                    case TokenType.Digit: return "digit";
                    case TokenType.TypeList: return "type-list";
                    case TokenType.Node: return "node";
                    case TokenType.Failure: return "none";
                    default: return "";
                }
            }
        }
    }
}
