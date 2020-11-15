#pragma once

#include <string>
#include <vector>
#include <utility>
#include <functional>

namespace good_basic {
    namespace parser_helpers {
        inline bool isDigit(const char c) {
            return c >= '0' && c <= '9';
        }
    }

    namespace parser {
        template<typename ast>
        using ParseResult = std::vector<std::pair<ast, std::string>>;

        template<typename ast>
        using Parser = std::function<ParseResult<ast>(const std::string&)>;

        template<typename ast>
        inline ParseResult<ast> parse(
                const Parser<ast>& parserFunc, const std::string& input) {
            return parserFunc(input);
        }

        extern const Parser<std::string> digit;
        extern const Parser<std::string> character(const char c);
    }
}
