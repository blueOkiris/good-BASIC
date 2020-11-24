using System;
using System.Collections.Generic;

namespace GoodBasic {
    namespace Parser {        
        partial class Parser {
            private string code;
            private RawToken[] lexemes;
            
            private int lexInd;
            private CompoundToken ast;
            
            private bool lexed;
            private bool parsed;
            
            public Parser(string code) {
                this.code = code;
                lexed = false;
                parsed = false;
            }
            
            public void Reset(string newCode) {
                code = newCode;
                lexed = false;
                parsed = false;
            }
            
            public CompoundToken Ast() {
                if(!lexed) {
                    lexInput();
                }
                if(!parsed) {
                    parseLexemes();
                }
                return ast;
            }
            
            public RawToken[] Lexemes() {
                if(!lexed) {
                    lexInput();
                }
                return lexemes;
            }
            
            private void lexInput() {
                var tokens = new List<RawToken>();
                var len = code.Length;
                for(int ind = 0, line = 1, col = 1; ind < len; ind++, col++) {
                    if(code[ind] == '\n') {
                        tokens.Add(
                            new RawToken(TokenType.NewLine, "\n", line, col)
                        );
                        line++;
                        col = 1 - 1;
                        continue;
                    } else if(ind + 1 < code.Length
                            && code[ind] == '\\' && code[ind + 1] == '\n') {
                        ind ++;
                        
                        line++;
                        col = 1 - 1;
                        continue;
                    } else if(char.IsWhiteSpace(code[ind])) {
                        continue;
                    }
                    
                    bool matchFound = false;
                    foreach(var pattern in regexesToTokenTypes.Keys) {
                        var match = pattern.Match(code.Substring(ind));
                        if(match.Success && match.Index == 0) {
                            tokens.Add(
                                new RawToken(
                                    regexesToTokenTypes[pattern], match.Value,
                                    line, col
                                )
                            );
                            col += match.Length - 1;
                            ind += match.Length - 1;
                            matchFound = true;
                            break;
                        }
                    }
                    
                    // All attempts failed, so just parse a single character
                    if(!matchFound) {
                        tokens.Add(
                            new RawToken(
                                TokenType.Symbol, "" + code[ind], line, col
                            )
                        );
                    }
                }
                lexemes = tokens.ToArray();
            }
            
            private void parseLexemes() {
                var children = new List<Token>();
                lexInd = 0;
                
                /*
                 * <module> ::= { <import> } /\n+/
                 *              <export> [ <implement> ] /\n+/
                 *              { <definition> /\n+/ }
                 */
                
                // Get all the imports
                while(lexInd < lexemes.Length
                        && (string) lexemes[lexInd].Source() == "imports") {
                    var import = parseImport();
                    if(lexInd >= lexemes.Length) {
                        throw new UnexpectedEOFException(import.Line());
                    } else if(lexemes[lexInd].Type() != TokenType.NewLine) {
                        throw new UnexpectedTokenException(
                            lexemes[lexInd].Type(),
                            new TokenType[] { TokenType.NewLine },
                            import.Line()
                        );
                    }
                    var newLine = lexemes[lexInd++];
                    eatNewLines();
                    
                    children.Add(import);
                    children.Add(newLine);
                }
                
                // Get the exports and implements line
                var export = parseExport();
                children.Add(export);
                if(lexInd >= lexemes.Length) {
                    throw new UnexpectedEOFException(export.Line());
                } else if((string) lexemes[lexInd].Source() == "implements") {
                    children.Add(parseImplement()); // Optional, hence if stmt
                }
                if(lexInd >= lexemes.Length) {
                    throw new UnexpectedEOFException(export.Line());
                } else if(lexemes[lexInd].Type() != TokenType.NewLine) {
                    throw new UnexpectedTokenException(
                        lexemes[lexInd].Type(),
                        new TokenType[] { TokenType.NewLine },
                        export.Line()
                    );
                }
                children.Add(lexemes[lexInd++]);
                eatNewLines();
                
                // Finally, parse definitions
                while(lexInd < lexemes.Length) {
                    var definition = parseDefinition();
                    if(lexInd >= lexemes.Length) {
                        throw new UnexpectedEOFException(definition.Line());
                    } else if(lexemes[lexInd].Type() != TokenType.NewLine) {
                        throw new UnexpectedTokenException(
                            lexemes[lexInd].Type(),
                            new TokenType[] { TokenType.NewLine },
                            definition.Line()
                        );
                    }
                    var newLine3 = lexemes[lexInd++];
                    eatNewLines();
                    
                    children.Add(definition);
                    children.Add(newLine3);
                }
                
                ast = new CompoundToken(TokenType.Module, children, 1);
            }
            
            private void eatNewLines() {
                while(lexInd < lexemes.Length
                        && lexemes[lexInd].Type() == TokenType.NewLine) {
                    lexInd++;
                }
            }
        }
    }
}
