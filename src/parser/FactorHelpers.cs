using System.Collections.Generic;

namespace GoodBasic {
    namespace Parser {
        partial class Parser {
            // <mem-acc> ::= <ident> ':' ( <ident> | <member-acc> )
            private CompoundToken parseMemAcc() {
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
                
                if(lexInd >= lexemes.Length) {
                    throw new UnexpectedEOFException(-1);
                } else if((string) lexemes[lexInd].Source() != ":") {
                    throw new UnexpectedTokenException(
                        lexemes[lexInd].Type(),
                        new TokenType[] { TokenType.Symbol },
                        lexemes[lexInd].Line()
                    );
                }
                children.Add(lexemes[lexInd++]);
                
                var startingLexInd = lexInd;
                try {
                    var nextMemAcc = parseMemAcc();
                    children.Add(nextMemAcc);
                } catch(ParserException) {
                    lexInd = startingLexInd;
                    if(lexInd >= lexemes.Length) {
                        throw new UnexpectedEOFException(-1);
                    } else if(lexemes[lexInd].Type() != TokenType.Identifier) {
                        throw new UnexpectedTokenException(
                            lexemes[lexInd].Type(),
                            new TokenType[] { TokenType.Identifier },
                            lexemes[lexInd].Line()
                        );
                    }
                    children.Add(lexemes[lexInd++]);
                }
                
                return new CompoundToken(
                    TokenType.MemberAcc, children, firstIdent.Line()
                );
            }
        }
    }
}
