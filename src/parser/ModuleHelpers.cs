using System.Collections.Generic;

namespace GoodBasic {
    namespace Parser {
        partial class Parser {
            // <import> ::= 'imports' <ident>
            private CompoundToken parseImport() {
                if(lexInd >= lexemes.Length) {
                    throw new UnexpectedEOFException(-1); // shouldn't happen
                } else if((string) lexemes[lexInd].Source() != "imports") {
                    throw new UnexpectedTokenException(
                        lexemes[lexInd].Type(),
                        new TokenType[] { TokenType.Keyword },
                        lexemes[lexInd].Line()
                    );
                }
                var importsKeyword = lexemes[lexInd++];
                
                if(lexInd >= lexemes.Length) {
                    throw new UnexpectedEOFException(importsKeyword.Line());
                } else if(lexemes[lexInd].Type() != TokenType.Identifier) {
                    throw new UnexpectedTokenException(
                        lexemes[lexInd].Type(),
                        new TokenType[] { TokenType.Identifier },
                        lexemes[lexInd].Line()
                    );
                }
                var name = lexemes[lexInd++];
                
                return new CompoundToken(
                    TokenType.Import,
                    new List<Token> { importsKeyword, name },
                    importsKeyword.Line()
                );
            }
            
            private CompoundToken parseExport() {
                if(lexInd >= lexemes.Length) {
                    throw new UnexpectedEOFException(-1); // shouldn't happen
                } else if((string) lexemes[lexInd].Source() != "exports") {
                    throw new UnexpectedTokenException(
                        lexemes[lexInd].Type(),
                        new TokenType[] { TokenType.Keyword },
                        lexemes[lexInd].Line()
                    );
                }
                var exportsKeyword = lexemes[lexInd++];
                var list = parseIdentList();
                return new CompoundToken(
                    TokenType.Export, new List<Token> { exportsKeyword, list },
                    exportsKeyword.Line()
                );
            }
            
            private CompoundToken parseImplement() {
                return new CompoundToken(
                    TokenType.Implement, new List<Token> {}, -1
                );
            }
            
            private CompoundToken parseDefinition() {
                return new CompoundToken(
                    TokenType.Definition, new List<Token> {}, -1
                );
            }
            
            private CompoundToken parseIdentList() {
                if(lexInd >= lexemes.Length) {
                    throw new UnexpectedEOFException(-1); // shouldn't happen
                } else if(lexemes[lexInd].Type() != TokenType.Identifier) {
                    throw new UnexpectedTokenException(
                        lexemes[lexInd].Type(),
                        new TokenType[] { TokenType.Identifier },
                        lexemes[lexInd].Line()
                    );
                }
                var firstIdent = lexemes[lexInd++];
                
                var children = new List<Token>();
                children.Add(firstIdent);
                while(lexInd < lexemes.Length
                        && (string) lexemes[lexInd].Source() == ",") {
                    var comma = lexemes[lexInd++];
                    if(lexInd >= lexemes.Length) {
                        throw new UnexpectedEOFException(firstIdent.Line());
                    } else if(lexemes[lexInd].Type() != TokenType.Identifier) {
                        throw new UnexpectedTokenException(
                            lexemes[lexInd].Type(),
                            new TokenType[] { TokenType.Identifier },
                            lexemes[lexInd].Line()
                        );
                    }
                    var ident = lexemes[lexInd++];
                    
                    children.Add(comma);
                    children.Add(ident);
                }
                
                return new CompoundToken(
                    TokenType.IdentList, children, firstIdent.Line()
                );
            }
        }
    }
}
