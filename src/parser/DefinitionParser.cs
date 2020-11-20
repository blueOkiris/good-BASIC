using System.Collections.Generic;

namespace GoodBasic {
    namespace Parser {
        // <definition> ::= <func-def> | <comp-def> | <rec-def>
        class Definition : Parser {
            public (Token, string) Parse(string input) =>
                new SelectFrom {
                    new FuncDef(), new CompDef(), new RecDef()
                }.Parse(input);
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Definition };
        }
        
        /*
         * <func-def> ::= 'def' 'fn' <ident> '(' [ <type-arg-list> ] ')' 
         *                       ( <type-name> | 'void' ) /\n+/
         *                    { <statement> /\n+/ } /\n+/
         *                '<end>'
         */
        class FuncDef : Parser {
            public (Token, string) Parse(string input) {
                var defKeyword = new Word("def").Parse(input);
                var fnKeyword = new Word("fn").Parse(defKeyword.Item2);
                var lpar = new Char('(').Parse(fnKeyword.Item2);
                var argList = new Maybe(new TypeArgList()).Parse(lpar.Item2);
                var rpar = new Char(')').Parse(argList.Item2);
                var retType = new SelectFrom {
                    new TypeName(), new Word("void")
                }.Parse(rpar.Item2);
                var stmts = new Maybe(
                    new Many(new Statement())
                ).Parse(retType.Item2);
                var endKeyword = new Word("end").Parse(stmts.Item2);
                
                var funcDef = defKeyword.Item1 + fnKeyword.Item1 + lpar.Item1;
                if(argList.Item1.type != TokenType.Failure) {
                    funcDef += argList.Item1;
                }
                funcDef += rpar.Item1;
                if(stmts.Item1.type != TokenType.Failure) {
                    funcDef += stmts.Item1;
                }
                funcDef += endKeyword.Item1;
                funcDef.type = TokenType.FuncDef;
                
                return (funcDef, endKeyword.Item2);
            }
            
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.FuncDef };
        }
        
        // <type-arg-list> ::= <type-name> <ident> { ',' <type-name> <ident> }
        class TypeArgList : Parser {
            public (Token, string) Parse(string input) {
                var typeName = new TypeName().Parse(input);
                var name = new Ident().Parse(typeName.Item2);
                var suffix = new Maybe(
                    new Create(TokenType.Node) {
                        new Char(','), new TypeName(), new Ident()
                    }
                ).Parse(name.Item2);
                
                var typeArgList = typeName.Item1 + name.Item1;
                if(suffix.Item1.type != TokenType.Failure) {
                    typeArgList += suffix.Item1;
                }
                typeArgList.type = TokenType.TypeArgList;
                
                return (typeArgList, suffix.Item2);
            }
            
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.TypeArgList };
        }
        
        /*
         * <type-name> ::= 'int' | 'float' | 'str'
         *               | 'fn' '(' [ <type-list> ] ')' <type-name>
         *               | '(' <type-name> ')'
         *               | '[' <type-name> ']'
         *               | <ident>
         *               | 'mut' <type-name>
         */
        class TypeName : Parser {
            public (Token, string) Parse(string input) {
                var mutKeyword = new Maybe(new Word("mut")).Parse(input);
                var baseTypeName = new SelectFrom {
                    new Create(TokenType.TypeName) {
                        new Word("fn"), new Char('('), new TypeList(),
                        new Char(')'), new TypeName()
                    }, new Word("int"), new Word("float"), new Word("str"),
                    new Ident(),
                    new Create(TokenType.TypeName) {
                        new Char('('), new TypeName(), new Char(')')
                    }, new Create(TokenType.TypeName) {
                        new Char('['), new TypeName(), new Char(']')
                    }
                }.Parse(mutKeyword.Item2);
                
                var typeName = baseTypeName.Item1;
                if(mutKeyword.Item1.type != TokenType.Failure) {
                    typeName = mutKeyword.Item1 + typeName;
                }
                typeName.type = TokenType.TypeName;
                
                return (typeName, baseTypeName.Item2);
            }
            
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.TypeName };
        }
        
        // <type-list> ::= <type-name> { ',' <type-name> }
        class TypeList : Parser {
            public (Token, string) Parse(string input) {
                var firstType = new TypeName().Parse(input);
                var suffix = new Maybe(
                    new Many(
                        new Create(TokenType.Node) {
                            new Char(','), new TypeName()
                        }
                    )
                ).Parse(firstType.Item2);
                
                var typeList = firstType.Item1;
                if(suffix.Item1.type != TokenType.Failure) {
                    typeList += suffix.Item1;
                }
                typeList.type = TokenType.TypeList;
                
                return (typeList, suffix.Item2);
            }
            
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.TypeList };
        }
        
        /*
         * <comp-def> ::= 'def' 'comp' <ident>  /\n+/
         *                    { <type-name> <ident> /\n+/ }
         *                'end'
         */
        class CompDef : Parser {
            public (Token, string) Parse(string input) {
                var defKeyword = new Word("def").Parse(input);
                var compKeyword = new Word("comp").Parse(defKeyword.Item2);
                var name = new Ident().Parse(compKeyword.Item2);
                var functions = new Maybe(
                    new Many(
                        new Create(TokenType.Node) {
                            new TypeName(), new Ident()
                        }
                    )
                ).Parse(name.Item2);
                var endKeyword = new Word("end").Parse(functions.Item2);
                
                var token = defKeyword.Item1 + compKeyword.Item1 + name.Item1;
                if(functions.Item1.type != TokenType.Failure) {
                    token += functions.Item1;
                }
                token += endKeyword.Item1;
                token.type = TokenType.CompDef;
                
                return (token, endKeyword.Item2);
            }
            
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.CompDef };
        }
        
        /*
         * <rec-def> ::= 'def' 'rec' <ident> /\n+/
         *                   { <ident> '(' [ <type-arg-list> ] ')' <type-name> }
         *               'end'
         */
        class RecDef : Parser {
            public (Token, string) Parse(string input) {
                var defKeyword = new Word("def").Parse(input);
                var recKeyword = new Word("rec").Parse(defKeyword.Item2);
                var name = new Ident().Parse(recKeyword.Item2);
                var functions = new Maybe(
                    new Many(new MiniFuncDef())
                ).Parse(name.Item2);
                var endKeyword = new Word("end").Parse(functions.Item2);
                
                var token = defKeyword.Item1 + recKeyword.Item1 + name.Item1;
                if(functions.Item1.type != TokenType.Failure) {
                    token += functions.Item1;
                }
                token += endKeyword.Item1;
                token.type = TokenType.RecDef;
                
                return (token, endKeyword.Item2);
            }
            
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.RecDef };
        }
        
        // Helper for RecDef
        class MiniFuncDef : Parser {
            public (Token, string) Parse(string input) {
                var name = new Ident().Parse(input);
                var lpar = new Char('(').Parse(name.Item2);
                var argList = new Maybe(new TypeArgList()).Parse(lpar.Item2);
                var rpar = new Char(')').Parse(argList.Item2);
                var typeName = new TypeName().Parse(rpar.Item2);
                
                var token = name.Item1 + lpar.Item1;
                if(argList.Item1.type != TokenType.Failure) {
                    token += argList.Item1;
                }
                token += rpar.Item1 + typeName.Item1;
                token.type = TokenType.Node;
                
                return (token, typeName.Item2);
            }
            
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Node };
        }
    }
}