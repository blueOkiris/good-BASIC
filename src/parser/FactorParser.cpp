#include <parser/Parser.hpp>
#include <parser/FactorParser.hpp>

using namespace good_basic;
using namespace parser;

std::vector<TokenType> Ident::type() const {
    return { TokenType::Ident };
}

// <ident> ::= /[A-Za-z_][A-Za-z0-9_]+/
ParserResult Ident::parse(const std::string& input) const {
    std::vector<ParserPtr> fstCharOpts = { sptr(Alpha()), sptr(Char('_')) };
    std::vector<ParserPtr> sndCharOpts = {
        sptr(Alpha()), sptr(Char('_')), sptr(Digit())
    };
    std::vector<ParserPtr> multiCharIdentSteps = {
        sptr(SelectFrom(fstCharOpts)),
        sptr(Many(sptr(SelectFrom(sndCharOpts))))
    };
    auto multiCharIdent = CreateFrom(multiCharIdentSteps, TokenType::Ident);
    auto singleCharIdent = AsType(
        sptr(SelectFrom(fstCharOpts)), TokenType::Ident
    );
    std::vector<ParserPtr> parserOpts = {
        sptr(multiCharIdent), sptr(singleCharIdent)
    };
    auto parser = SelectFrom(parserOpts);
    return parser.parse(input);
}
