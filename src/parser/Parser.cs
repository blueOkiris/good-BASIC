using System;
using System.Collections.Generic;

namespace GoodBasic {
    namespace Parser {        
        partial class Parser {
            private string code;
            private int codeInd;
            private RawToken[] lexemes;
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
                        col = 1;
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
                
            }
        }
    }
}
