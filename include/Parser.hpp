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
        using ParseResult = std::pair<std::string, std::string>;
        using Parser = std::function<ParseResult(const std::string&)>;

        inline ParseResult parse(
                const Parser& parserFunc, const std::string& input) {
            return parserFunc(input);
        }

        extern const Parser digit;
        extern const Parser character(const char c);
        extern const Parser some(const Parser& parserFunc);
        extern const Parser either(
            const Parser &parser1, const Parser& parser2
        );

        extern const Parser integer;
    }
}
