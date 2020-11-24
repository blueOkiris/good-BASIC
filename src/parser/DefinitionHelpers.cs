using System.Collections.Generic;

namespace GoodBasic {
    namespace Parser {
        partial class Parser {
            private CompoundToken parseFuncDef() {
                return new CompoundToken(
                    TokenType.FuncDef,
                    new List<Token> {},
                    -1
                );
            }
            
            private CompoundToken parseCompDef() {
                return new CompoundToken(
                    TokenType.CompDef,
                    new List<Token> {},
                    -1
                );
            }
            
            private CompoundToken parseRecDef() {
                return new CompoundToken(
                    TokenType.RecDef,
                    new List<Token> {},
                    -1
                );
            }
        }
    }
}
