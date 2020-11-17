using System;
using System.Collections.Generic;

namespace GoodBasic {
    namespace Parser {
        class Ident : Parser {
            public (Token, string) Parse(string input) =>
                new SelectFrom(new List<Parser> {
                    new CreateFrom(
                        new List<Parser> {
                            new SelectFrom(new List<Parser> {
                                new Alpha(), new Char('_')
                            }), new Many(
                                new SelectFrom(new List<Parser> {
                                    new Alpha(), new Char('_'),
                                    new Digit()
                                })
                            )
                        }, TokenType.Ident
                    ), new AsType(
                        new SelectFrom(new List<Parser> {
                            new Alpha(), new Char('_')
                        }), TokenType.Ident
                    )
                }).Parse(input);
            public List<TokenType> Types() =>
                new List<TokenType> { TokenType.Ident };
        }
    }
}