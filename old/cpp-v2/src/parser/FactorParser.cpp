#include <parser/Parser.hpp>
#include <parser/FactorParser.hpp>

using namespace good_basic;
using namespace parser;

std::vector<TokenType> Ident::type() const {
    return { TokenType::Ident };
}

// <ident> ::= /[A-Za-z_][A-Za-z0-9_]+/
ParserResult Ident::parse(const std::string& input) const {
    const auto parser = SelectFrom({
        sptr(CreateFrom(
            {
                sptr(SelectFrom({ sptr(Alpha()), sptr(Char('_')) })),
                sptr(Many(
                    sptr(SelectFrom(
                        { sptr(Alpha()), sptr(Char('_')), sptr(Digit()) }
                    ))
                ))
            }, TokenType::Ident
        )), sptr(AsType(
            sptr(SelectFrom({ sptr(Alpha()), sptr(Char('_')) })),
            TokenType::Ident
        ))
    });
    return parser.parse(input);
}
