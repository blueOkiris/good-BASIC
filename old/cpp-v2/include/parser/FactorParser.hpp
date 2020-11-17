#pragma once

#include <parser/Parser.hpp>

/*
 * <factor>        ::= <ident> | <int> | <float> | <string>
 *                   | lambda | <comp-rec-dec>
 *                   | <member-acc> | <func-call> | '(' <expr> ')'
 * <member-acc>    ::= <ident> ':' ( <ident> | <member-acc> )
 * <func-call>     ::= 'call' <ident> { <expr> }
 * <lambda>        ::= 'lambda' '(' [ <type-arg-list> ] ')' <type-name> /\n+/
 *                         { <statement> /\n+/ }
 *                     'end'
 * <comp-rec-dec>  ::= 'data' <ident> '(' [ <expr> { ',' <expr> } ] ')'
 * 
 * <ident>         ::= /[A-Za-z_][A-Za-z0-9_]+/
 * <float>         ::= /-?((.[0-9]+)|([0-9]+.)|([0-9]+.[0-9]+))(e[+-]?[0-9]+)/
 * <int>           ::= /-?[0-9]+/
 * <string>        ::= /'(\\.|[^\\'])*'/
*/

namespace good_basic {
    namespace parser {
        class Ident : public Parser {
            public:
                std::vector<TokenType> type() const override;
                ParserResult parse(const std::string& input) const override;
        };
    }
}