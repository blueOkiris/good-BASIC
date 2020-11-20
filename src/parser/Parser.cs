using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Text;

namespace GoodBasic {
    namespace Parser {
        static class Lexer {
            public static List<string> Keywords = new List<string> {
                "imports", "exports", "implements",
                "def", "fn", "comp", "comp", "rec", "end",
                "let", "int", "float", "str", "mut",
                "return", "call", "lambda", "data"
            };
            
            private static Regex identRegex = new Regex(
                @"[A-Za-z_][A-Za-z0-9_]+", RegexOptions.Compiled
            );
            private static Regex floatRegex = new Regex(
                @"[\+-]?((.[0-9]+)|([0-9]+.)|([0-9]+.[0-9]+))(e[\+-]?[0-9]+)",
                RegexOptions.Compiled
            );
            private static Regex intRegex = new Regex(
                @"[\+-]?[0-9]+", RegexOptions.Compiled
            );
            private static Regex strRegex = new Regex(
                @"'(\\.|[^\\'])*'", RegexOptions.Compiled
            );
            
            public static List<RawToken> Lex(string code) {
                var tokens = new List<RawToken>();
                for(int i = 0; i < code.Length; i++) {
                    if(code[i] == '\n') {
                        tokens.Add(new RawToken(TokenType.NewLine, "\n"));
                        continue;
                    } else if(code[i] == '\\'
                            && i + 1 < code.Length && code[i + 1] == '\n') {
                        i++;
                        continue;
                    } else if(char.IsWhiteSpace(code[i])) {
                        continue;
                    }
                    
                    var identResult = identRegex.Match(code.Substring(i));
                    var floatResult = floatRegex.Match(code.Substring(i));
                    var intResult = intRegex.Match(code.Substring(i));
                    var strResult = strRegex.Match(code.Substring(i));
                    if(code.StartsWith(identResult.Value)
                            && identResult.Value != "") {
                        tokens.Add(
                            new RawToken(
                                Keywords.Contains(identResult.Value) ?
                                    TokenType.Keyword :
                                    TokenType.Identifier,
                                identResult.Value
                            )
                        );
                        i += identResult.Value.Length + 1;
                    } else if(code.StartsWith(floatResult.Value)
                            && floatResult.Value != "") {
                        tokens.Add(
                            new RawToken(
                                TokenType.FloatingPoint, floatResult.Value
                            )
                        );
                        i += floatResult.Value.Length + 1;
                    } else if(code.StartsWith(intResult.Value)
                            && intResult.Value != "") {
                        tokens.Add(
                            new RawToken(TokenType.Integer, intResult.Value)
                        );
                        i += intResult.Value.Length + 1;
                    } else if(code.StartsWith(strResult.Value)
                            && strResult.Value != "") {
                        tokens.Add(
                            new RawToken(TokenType.String, strResult.Value)
                        );
                        i += strResult.Value.Length + 1;
                    } else {
                        tokens.Add(
                            new RawToken(TokenType.Character, "" + code[i])
                        );
                    }
                }
                return tokens;
            }
        }
        
        static partial class Parser {
            public static List<T> Sublist<T>(this List<T> list, int startPos) {
                var listCopy = new List<T>();
                for(int i = startPos; i < list.Count; i++) {
                    listCopy.Add(list[i]);
                }
                return listCopy;
            }
            
            public static CompoundToken Parse(List<RawToken> tokens) =>
                parseModule(tokens);
        }
        
        class ParserException : Exception {
            public ParserException(string message) : base(message) {
            }
        }
        
        class UnexpectedTypeException : ParserException {
            public UnexpectedTypeException(TokenType expectedType) :
                    base(typesToMessage(expectedType)) {
            }
            
            public static string typesToMessage(TokenType type) {
                var msg = new StringBuilder(
                    "Parser Exception: Unexpected type! "
                    + "Expected Types: "
                );
                msg.Append(type);
                return msg.ToString();
            }
            
            public UnexpectedTypeException(List<TokenType> expectedTypes) :
                    base(typesToMessage(expectedTypes)) {
            }
            
            public static string typesToMessage(List<TokenType> types) {
                var msg = new StringBuilder(
                    "Parser Exception: Unexpected type! "
                    + "Expected Types: "
                );
                foreach(var type in types) {
                    msg.Append(type).Append(' ');
                }
                return msg.ToString();
            }
        }
    }
}