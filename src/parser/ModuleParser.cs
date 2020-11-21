using System.Collections.Generic;

namespace GoodBasic {
    namespace Parser {
        static partial class Parser {
            private static CompoundToken parseModule(List<RawToken> tokens) {
                if(tokens.Count == 0) {
                    throw new UnexpectedTypeException(TokenType.Module);
                }
                
                var children = new List<Token>();
                
                int i = 1;
                while(i < tokens.Count
                        && tokens[i].Source() as string == "imports") {
                    var import = parseImport(tokens.Sublist(i));
                    children.Add(import.Item1);
                    i += import.Item2;
                    if(i >= tokens.Count
                            || tokens[i].Type() != TokenType.NewLine) {
                        throw new UnexpectedTypeException(TokenType.NewLine);
                    }
                    i++;
                }
                var export = parseExport(tokens.Sublist(i));
                children.Add(export.Item1);
                i += export.Item2;
                var implement = parseImplement(tokens.Sublist(i));
                children.Add(implement.Item1);
                i += implement.Item2;
                
                while(i < tokens.Count) {
                    var definition = parseDefinition(tokens.Sublist(i));
                    children.Add(implement.Item1);
                    i += definition.Item2;
                    if(i >= tokens.Count
                            || tokens[i].Type() != TokenType.NewLine) {
                        throw new UnexpectedTypeException(TokenType.NewLine);
                    }
                    i++;
                }
                
                return new CompoundToken(
                    TokenType.Module, children, children[0].Line()
                );
            }
        }
    }
}