using System.Collections.Generic;

namespace GoodBasic {
    namespace Parser {
        partial class Parser {
            private CompoundToken parseImport() {
                return new CompoundToken(
                    TokenType.Import, new List<Token> {}, 0
                );
            }
            
            private CompoundToken parseExport() {
                return new CompoundToken(
                    TokenType.Export, new List<Token> {}, 0
                );
            }
            
            private CompoundToken parseImplement() {
                return new CompoundToken(
                    TokenType.Implement, new List<Token> {}, 0
                );
            }
            
            private CompoundToken parseDefinition() {
                return new CompoundToken(
                    TokenType.Definition, new List<Token> {}, 0
                );
            }
        }
    }
}
