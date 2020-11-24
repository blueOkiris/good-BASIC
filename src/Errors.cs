using System;
using System.Text;

namespace GoodBasic {
    namespace Parser {
        class ParserException : Exception {
            public ParserException(string message) :
                    base("Parser Exception: " + message) {
            }
        }
        
        class UnexpectedEOFException : ParserException {
            public UnexpectedEOFException(int line) :
                    base("Unexpected EOF on line " + line) {
            }
        }
        
        class UnexpectedTokenException : ParserException {
            private static string expectedTypesStr(TokenType[] expected) {
                if(expected.Length == 0) {
                    return "";
                }
                var typesStr = new StringBuilder();
                typesStr.Append(expected[0]);
                for(int i = 1; i < expected.Length; i++) {
                    typesStr.Append(", ");
                    typesStr.Append(expected[i]);
                }
                return typesStr.ToString();
            }
            public UnexpectedTokenException(
                    TokenType type, TokenType[] expected, int line) :
                    base(
                        "Unepected token of type " + type
                            + " on line " + line + ". "
                            + "Allowed types: " + expectedTypesStr(expected)
                    ) {
            }
        }
    }
}
