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
        }
        return ParseResult({ success.str(), result.second });
    };
}

const Parser parser::either(const Parser& parser1, const Parser& parser2) {
    return [parser1, parser2](const std::string& input) {
        const auto try1 = parse(parser1, input);
        if(try1.first == "") {
            return parse(parser2, input);
        } else {
            return try1;
        }
    };
}

const Parser parser::integer = [](const std::string &input) {
    return parse(either(
        [](const std::string& input) {
            std::stringstream negativeNumber;
            const auto negSymb = parse(character('-'), input);
            if(negSymb.first == "") {
                return ParseResult({ "", "" });
            }
            const auto natNum = parse(some(digit), negSymb.second);
            if(natNum.first == "") {
                return ParseResult({ "", "" });
            }
            negativeNumber << '-' << natNum.first;
            return ParseResult({ negativeNumber.str(), natNum.second });
        }, some(digit)
    ), input);
};
