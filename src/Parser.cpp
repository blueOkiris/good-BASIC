#include <string>
#include <vector>
#include <utility>
#include <functional>
#include <Parser.hpp>

using namespace good_basic;
using namespace parser;

const Parser<std::string> parser::digit = [](const std::string& input) {
    if(input.length() < 1 || !parser_helpers::isDigit(input[0])) {
        return ParseResult<std::string>();
    } else {
        return ParseResult<std::string>(
            { { input.substr(0, 1), input.substr(1) } }
        );
    }
};

const Parser<std::string> parser::character(const char c) {
    return [c](const std::string& input) {
        if(input.length() < 1 || input[0] != c) {
            return ParseResult<std::string>();
        } else {
            return ParseResult<std::string>(
                { { input.substr(0, 1), input.substr(1) } }
            );
        }
    };
}
