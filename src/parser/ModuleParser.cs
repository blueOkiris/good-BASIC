using System.Collections.Generic;

namespace GoodBasic {
    namespace Parser {
        /*
         * <module> ::= { <import> } /\n+/
         *              <export> [ <implement> ] /\n+/
         *              { <definition> /\n+/ }
         */
        class Module : Parser {
            public (Token, string) Parse(string input) {
                var imports = new Maybe(
                    new Many(
                        new Create(TokenType.Import) {
                            new SkipWhitespace(), new Import(),
                            new SkipWhitespace(), new Many(new Char('\n'))
                        }
                    )
                ).Parse(input);
                    var sp1 = new SkipWhitespace().Parse(imports.Item2);
                var export = new Export().Parse(sp1.Item2);
                    var sp2 = new SkipWhitespace().Parse(export.Item2);
                var implement = new Maybe(new Implement()).Parse(sp2.Item2);
                    var sp3 = new SkipWhitespace().Parse(implement.Item2);
                var newLine = new Many(new Char('\n')).Parse(sp3.Item2);
                var definitions = new Maybe(
                    new Many(
                        new Create(TokenType.Definition) {
                            new SkipWhitespace(), new Definition(),
                            new SkipWhitespace(), new Many(new Char('\n'))
                        }
                    )
                ).Parse(newLine.Item2);
                
                var module = export.Item1;
                if(imports.Item1.type != TokenType.Failure) {
                    module = imports.Item1 + module;
                }
                if(implement.Item1.type != TokenType.Failure) {
                    module += implement.Item1;
                }
                if(definitions.Item1.type != TokenType.Failure) {
                    module += definitions.Item1;
                }
                
                return (module, definitions.Item2);
            }
            
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Module };
        }
        
        // <import> ::= 'import' <ident>
        class Import : Parser {
            public (Token, string) Parse(string input) =>
                new Create(TokenType.Import) {
                    new Word("import"), new SkipWhitespace(), new Ident()
                }.Parse(input);
            
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Import };
        }
        
        // <export> ::= 'exports' <ident-list>
        class Export : Parser {
            public (Token, string) Parse(string input) {
                var exportKeyword = new Word("exports").Parse(input);
                    var sp1 = new SkipWhitespace().Parse(exportKeyword.Item2);
                var list = new IdentList().Parse(sp1.Item2);
                
                var export = exportKeyword.Item1 + list.Item1;
                export.type = TokenType.Export;
                
                return (export, list.Item2);
            }
            
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Export };
        }
        
        // <implement> ::= 'implements' <ident-list>
        class Implement : Parser {
            public (Token, string) Parse(string input) {
                var implementKeyword = new Word("implements").Parse(input);
                    var sp1 = new SkipWhitespace().Parse(
                        implementKeyword.Item2
                    );
                var list = new IdentList().Parse(sp1.Item2);
                
                var implement = implementKeyword.Item1 + list.Item1;
                implement.type = TokenType.Implement;
                
                return (implement, list.Item2);
            }
            
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Implement };
        }
        
        // <ident-list> ::= <ident> { ',' <ident> }
        class IdentList : Parser {
            public (Token, string) Parse(string input) {
                var name = new Ident().Parse(input);
                var suffix = new Maybe(
                    new Create(TokenType.Node) {
                        new SkipWhitespace(), new Char(','),
                        new SkipWhitespace(), new Ident()
                    }
                ).Parse(name.Item2);
                
                var list = name.Item1;
                if(suffix.Item1.type != TokenType.Failure) {
                    list += suffix.Item1;
                }
                list.type = TokenType.IdentList;
                
                return (list, suffix.Item2);
            }
            
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.IdentList };
        }
    }
}
