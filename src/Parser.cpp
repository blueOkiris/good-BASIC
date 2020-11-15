#include <string>
#include <vector>
#include <utility>
#include <functional>
#include <sstream>
#include <iostream>
#include <Parser.hpp>

using namespace good_basic;
using namespace parser;

const Parser parser::digit = [](const std::string& input) {
    if(input.length() < 1 || !parser_helpers::isDigit(input[0])) {
        return ParseResult({ "", input });
    } else {
        return ParseResult({ input.substr(0, 1), input.substr(1) });
    }
};

const Parser parser::character(const char c) {
    return [c](const std::string& input) {
        if(input.length() < 1 || input[0] != c) {
            return ParseResult({ "", input });
        } else {
            return ParseResult({ input.substr(0, 1), input.substr(1) });
        }
    };
}

const Parser parser::some(const Parser& parserFunc) {
    return [parserFunc](const std::string& input) {
        std::stringstream success;
        auto result = parse(parserFunc, input);
        while(result.first != "") {
            success << result.first;
            result = parse(parserFunc, result.second);
            std::cout << result.first << ", " << result.second << std::endl;
        }
        return ParseResult({ success.str(), result.second });
    };
}
