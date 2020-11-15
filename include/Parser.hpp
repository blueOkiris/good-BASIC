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

        inline bool isAlpha(const char c) {
            return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
        }
    }

    namespace parser {
        using ParseResult = std::pair<std::string, std::string>;
        using Parser = std::function<ParseResult(const std::string&)>;

        inline ParseResult parse(
                const Parser& parserFunc, const std::string& input) {
            return parserFunc(input);
        }

        Parser multiple(const Parser& parserFunc);
        Parser either(
            const Parser &parser1, const Parser& parser2
        );
        Parser doParsers(const std::vector<Parser>& steps);

        extern const Parser alpha;
        extern const Parser digit;
        extern const Parser anyChar;
        Parser character(const char c);
        Parser anyCharExcept(const std::vector<char>& options);

        extern const Parser integer;
        extern const Parser str;
        extern const Parser decimal;
        extern const Parser ident;
    }
}
